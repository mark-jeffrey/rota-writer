# Solving the rota model with HiGHS and writing the result.

#' Solve the rota
#'
#' Builds the model with [build_rota_model()] and solves it with the HiGHS
#' MILP solver in two phases:
#' 1. Find a rota that meets every hard rule with every slot filled. With
#'    workable inputs this takes seconds. If it cannot (the inputs are
#'    impossible, or it runs out of time), build the rota a week at a time
#'    instead, filling as many slots as the hard rules allow.
#' 2. Improve it by large neighbourhood search: repeatedly free one week, or
#'    a random group of doctors, fix the rest of the rota, and re-solve that
#'    part to lower the soft-rule penalties. Every step solves the full model,
#'    so the hard rules always hold and fairness is judged over the whole
#'    rota.
#'
#' If the inputs make some slots impossible to fill (e.g. too few doctors
#' with the right competencies, or everyone on leave), the rota leaves them
#' empty rather than break a contract rule, and `unfilled` lists them.
#'
#' Phase 2 runs until the time limit, until the penalty is 0, or until many
#' steps in a row fail to improve it. It does not prove the rota is the best
#' possible, but in practice gets close; give it longer for a better rota.
#' Check the result with [check_rota()].
#'
#' @inheritParams check_rota
#' @param time_limit Seconds to spend in total. Defaults to
#'   `solver_time_limit_s` in the `settings` sheet.
#' @param seed Random seed for phase 2, so the same inputs give the same rota.
#' @param verbose If `TRUE`, report each improvement as it is found.
#'
#' @returns A `rota_solution`: a list with
#'   \describe{
#'     \item{rota}{The rota in long format (`doctor_id`, `date`, `code`), as
#'       used by [check_rota()], or `NULL` if no rota was found.}
#'     \item{status}{`"Feasible"` if every slot is filled, `"Unfilled shifts"`
#'       if some could not be, otherwise the solver's reason for finding no
#'       rota, e.g. `"Time limit reached"`.}
#'     \item{unfilled}{Slots the rota leaves short: `date`, `shift_code`,
#'       `needed` and `filled`. Zero rows when every slot is filled.}
#'     \item{objective}{Total penalty of the rota.}
#'     \item{penalties}{Penalty by soft rule (`rule`, `penalty`). Apart from
#'       the `FAIR_*` rules, these match the totals from [check_rota()].}
#'     \item{history}{Penalty after each improvement (`seconds`,
#'       `objective`).}
#'     \item{seconds}{Time spent building and solving.}
#'     \item{model}{The `rota_model` that was solved.}
#'   }
#'
#' @seealso [write_rota()] to save the rota as a workbook.
#' @export
#' @examples
#' \dontrun{
#' inputs <- read_rota_inputs(system.file("extdata", "rota_inputs.xlsx", package = "rotawriter"))
#' solution <- solve_rota(inputs, time_limit = 120)
#' check_rota(solution$rota, inputs)
#' }
solve_rota <- function(inputs, time_limit = NULL, seed = 1, verbose = FALSE) {
  inputs <- as_rota_inputs(inputs)
  setting <- set_names(inputs$settings$value, inputs$settings$key)
  time_limit <- time_limit %||% as.numeric(setting["solver_time_limit_s"])
  if (is.na(time_limit)) {
    time_limit <- Inf
  }
  if (!is.na(setting["solver"]) && tolower(setting[["solver"]]) != "highs") {
    warning("Only the HiGHS solver is supported so far; using it instead of ",
            setting[["solver"]], call. = FALSE)
  }

  started <- Sys.time()
  elapsed <- \() as.numeric(difftime(Sys.time(), started, units = "secs"))
  model <- build_rota_model(inputs)
  solve <- highs_solver(model)

  # Phase 1: every slot filled, as a pure feasibility problem (minimising
  # unfilled slots instead is far slower). Proving it impossible can also be
  # slow, so cap it; an empty rota always meets the other hard rules.
  first <- solve(cost = rep(0, nrow(model$vars)), time_limit = min(time_limit / 4, 60),
                 allow_unfilled = FALSE)
  if (is_feasible(first)) {
    best <- polish(solve, model, first$primal_solution)
  } else {
    if (verbose) {
      message(sprintf("%5.0f s  cannot fill every slot (%s); filling as many as possible",
                      elapsed(), first$status_message))
    }
    best <- week_by_week(solve, model, deadline = \() time_limit - elapsed())
  }
  history <- tibble::tibble(seconds = elapsed(), objective = best$objective_value)
  if (verbose) message(sprintf("%5.0f s  first rota, penalty %g", elapsed(), best$objective_value))

  withr::with_seed(seed, {
    stalled <- 0
    step <- 0
    while (time_limit - elapsed() > 1 && best$objective_value > 1e-6 && stalled < 40) {
      step <- step + 1
      free <- neighbourhood(model, step)
      tried <- solve(cost = model$vars$cost, time_limit = min(5, time_limit - elapsed()),
                     fix = !free, current = best$primal_solution)
      if (is_feasible(tried) && tried$objective_value < best$objective_value - 1e-6) {
        best <- tried
        stalled <- 0
        history <- tibble::add_row(history, seconds = elapsed(), objective = best$objective_value)
        if (verbose) message(sprintf("%5.0f s  penalty %g", elapsed(), best$objective_value))
      } else {
        stalled <- stalled + 1
      }
    }
  })

  # A step stopped by its time limit can leave helper variables above their
  # minimum; re-solve them with the rota fixed so the penalties are exact
  best <- polish(solve, model, best$primal_solution)
  rota_solution(model, inputs, best, NULL, started, history)
}

# A function that solves the model with the given costs, optionally fixing
# the shift variables in `fix` to their values in `current`, which is also
# passed as the starting point. `allow_unfilled = FALSE` makes cover exact.
highs_solver <- function(model) {
  A <- Matrix::sparseMatrix(
    i = model$terms$i, j = model$terms$j, x = model$terms$coef,
    dims = c(nrow(model$rows), nrow(model$vars))
  )
  is_x <- model$vars$kind == "x"
  unfilled_j <- model$costs$j[model$costs$rule == "COVER"]

  function(cost, time_limit, fix = NULL, current = NULL, allow_unfilled = TRUE) {
    lower <- model$vars$lower
    upper <- model$vars$upper
    if (!allow_unfilled) {
      upper[unfilled_j] <- 0
    }
    start <- NULL
    if (!is.null(current)) {
      # Only the shift variables: HiGHS works out the helper variables
      start <- if_else(is_x, round(current), NA_real_)
      fixed <- is_x & fix
      lower[fixed] <- upper[fixed] <- start[fixed]
    }
    highs::highs_solve(
      L = cost, lower = lower, upper = upper,
      A = A, lhs = model$rows$lo, rhs = model$rows$hi, types = model$vars$type,
      offset = sum(model$offset$value), start = start,
      control = highs::highs_control(time_limit = max(time_limit, 1), mip_rel_gap = 1e-4)
    )
  }
}

# A feasible point can also come back when the solver stops at the time limit
is_feasible <- function(result) {
  identical(result$info$primal_solution_status, "Feasible")
}

# Build a rota one week at a time, each week solved with the weeks after it
# still empty. An empty rota meets every hard rule except cover, so each
# step is feasible, and unfilled slots cost so much that each week fills as
# many as it can.
week_by_week <- function(solve, model, deadline) {
  t <- model$vars$t
  current <- rep(0, nrow(model$vars))
  for (first in seq(1, max(t, na.rm = TRUE), by = 7)) {
    free <- model$vars$kind == "x" & t >= first & t < first + 7
    tried <- solve(cost = model$vars$cost, time_limit = min(10, deadline()), fix = !free,
                   current = current)
    if (is_feasible(tried)) {
      current <- tried$primal_solution
    }
  }
  polish(solve, model, current)
}

# Fix every shift and solve for the cheapest helper variables
polish <- function(solve, model, current) {
  polished <- solve(cost = model$vars$cost, time_limit = 60, fix = rep(TRUE, nrow(model$vars)),
                    current = current)
  stopifnot(is_feasible(polished))
  polished
}

# Shift variables to free in phase 2: alternately one random week, and a
# random group of up to 10 doctors for the whole rota
neighbourhood <- function(model, step, days = 7, doctors = 10) {
  is_x <- model$vars$kind == "x"
  t <- model$vars$t
  if (step %% 2 == 1) {
    first <- sample.int(max(max(t, na.rm = TRUE) - days + 1, 1), 1)
    is_x & t >= first & t < first + days
  } else {
    ids <- unique(model$vars$doctor_id[is_x])
    is_x & model$vars$doctor_id %in% sample(ids, min(doctors, length(ids)))
  }
}

rota_solution <- function(model, inputs, result, status, started, history = NULL) {
  rota <- penalties <- unfilled <- NULL
  if (!is.null(result)) {
    value <- result$primal_solution
    rota <- model$vars |>
      filter(kind == "x", value[j] > 0.5) |>
      transmute(doctor_id, date, code = shift_code) |>
      arrange(date, doctor_id)
    penalties <- bind_rows(
      transmute(model$costs, rule, penalty = coef * value[j]),
      rename(model$offset, penalty = value)
    ) |>
      summarise(penalty = round(sum(penalty), 6), .by = rule) |>
      arrange(desc(penalty))
    unfilled <- expected_cover(inputs) |>
      filter(n_min > 0) |>
      left_join(count(rota, date, shift_code = code, name = "filled"), by = c("date", "shift_code")) |>
      transmute(date, shift_code, needed = n_min, filled = coalesce(filled, 0L)) |>
      filter(filled < needed) |>
      arrange(date, shift_code)
    status <- if (nrow(unfilled) == 0) "Feasible" else "Unfilled shifts"
  }

  structure(
    list(
      rota = rota,
      status = status,
      objective = if (!is.null(result)) result$objective_value else NA_real_,
      unfilled = unfilled,
      penalties = penalties,
      history = history,
      seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      model = model
    ),
    class = c("rota_solution", "list")
  )
}

#' @export
print.rota_solution <- function(x, ...) {
  cat(sprintf("<rota_solution> %s after %.1f s\n", x$status, x$seconds))
  if (!is.null(x$rota)) {
    cat(sprintf("  %d shifts for %d doctors, %s to %s\n", nrow(x$rota),
                n_distinct(x$rota$doctor_id), min(x$rota$date), max(x$rota$date)))
    if (nrow(x$unfilled) > 0) {
      shown <- head(x$unfilled, 10)
      cat(sprintf("  %d slots could not be filled:\n", nrow(x$unfilled)))
      cat(sprintf("    %s %-8s %d of %d\n", shown$date, shown$shift_code, shown$filled, shown$needed),
          sep = "")
      if (nrow(x$unfilled) > 10) cat("    ...\n")
    }
    cat(sprintf("  Penalty %g\n", x$objective))
    cat(sprintf("    %-16s %10g\n", x$penalties$rule, x$penalties$penalty), sep = "")
  } else {
    cat("  No rota found\n")
  }
  invisible(x)
}
