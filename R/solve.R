# Solving the rota model with HiGHS and writing the result.

#' Solve the rota
#'
#' Builds the model with [build_rota_model()] and solves it with the HiGHS
#' MILP solver in two phases:
#' 1. Find any rota that meets every hard rule. This takes seconds.
#' 2. Improve it by large neighbourhood search: repeatedly free one week, or
#'    a random group of doctors, fix the rest of the rota, and re-solve that
#'    part to lower the soft-rule penalties. Every step solves the full model,
#'    so the hard rules always hold and fairness is judged over the whole
#'    rota.
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
#'     \item{status}{`"Feasible"` if a rota was found, otherwise the solver's
#'       reason, e.g. `"Infeasible"` (the hard rules cannot all be met) or
#'       `"Time limit reached"`.}
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

  first <- solve(cost = rep(0, nrow(model$vars)), time_limit = time_limit)
  if (!is_feasible(first)) {
    return(rota_solution(model, NULL, first$status_message, started))
  }

  best <- polish(solve, model, first$primal_solution)
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
  rota_solution(model, best, "Feasible", started, history)
}

# A function that solves the model with the given costs, optionally fixing
# the shift variables in `fix` to their values in `current`, which is also
# passed as the starting point
highs_solver <- function(model) {
  A <- Matrix::sparseMatrix(
    i = model$terms$i, j = model$terms$j, x = model$terms$coef,
    dims = c(nrow(model$rows), nrow(model$vars))
  )
  is_x <- model$vars$kind == "x"

  function(cost, time_limit, fix = NULL, current = NULL) {
    lower <- model$vars$lower
    upper <- model$vars$upper
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

rota_solution <- function(model, result, status, started, history = NULL) {
  rota <- penalties <- NULL
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
  }

  structure(
    list(
      rota = rota,
      status = status,
      objective = if (!is.null(result)) result$objective_value else NA_real_,
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
    cat(sprintf("  Penalty %g\n", x$objective))
    cat(sprintf("    %-16s %10g\n", x$penalties$rule, x$penalties$penalty), sep = "")
  } else {
    cat("  No rota found\n")
  }
  invisible(x)
}

leave_codes <- c(annual = "AL", study = "SL", exam = "EX", parental = "PL")

#' Write a rota workbook
#'
#' Saves a rota in the same layout as `inst/extdata/example_rota_output.xlsx`,
#' so it can be read back with [read_rota()] and checked with [check_rota()]:
#' * `grid`: one row per date and one column per doctor, for people to read
#' * `long`: one row per doctor per day (`doctor_id`, `date`, `code`)
#'
#' Approved leave is added as `AL` (annual), `SL` (study), `EX` (exam) or
#' `PL` (parental) on days the doctor has no shift.
#'
#' @param rota A rota in long format, e.g. `solve_rota(inputs)$rota`.
#' @inheritParams check_rota
#' @param path Path of the `.xlsx` file to write. An existing file is
#'   overwritten.
#'
#' @returns `path`, invisibly.
#' @export
write_rota <- function(rota, inputs, path) {
  inputs <- as_rota_inputs(inputs)
  rota <- as_rota(rota)

  leave <- leave_days(inputs) |>
    filter(status == "approved") |>
    transmute(doctor_id, date, code = unname(leave_codes[leave_type])) |>
    distinct(doctor_id, date, .keep_all = TRUE)
  long <- bind_rows(rota, anti_join(leave, rota, by = c("doctor_id", "date"))) |>
    filter(date >= inputs$period$start, date <= inputs$period$end) |>
    arrange(date, doctor_id)

  grid <- long |>
    pivot_wider(names_from = doctor_id, values_from = code) |>
    right_join(tibble::tibble(date = rota_dates(inputs)), by = "date") |>
    arrange(date) |>
    mutate(day = as.character(lubridate::wday(date, label = TRUE, locale = "C")), .after = date) |>
    select(date, day, any_of(inputs$staff$doctor_id))

  add_sheet <- function(wb, name, x) {
    wb |>
      openxlsx2::wb_add_worksheet(name) |>
      openxlsx2::wb_add_data_table(name, x, table_style = "TableStyleMedium2", na.strings = "") |>
      openxlsx2::wb_freeze_pane(name, first_row = TRUE) |>
      openxlsx2::wb_set_col_widths(name, cols = seq_along(x), widths = "auto")
  }
  grid_dims <- openxlsx2::wb_dims(rows = seq_len(nrow(grid)) + 1, cols = 3:max(3, ncol(grid)))
  highlight <- function(wb, text, style) {
    openxlsx2::wb_add_conditional_formatting(wb, "grid", dims = grid_dims, type = "containsText",
                                             rule = text, style = style)
  }

  wb <- openxlsx2::wb_workbook(creator = "rota-writer") |>
    openxlsx2::wb_add_dxfs_style(name = "night", bg_fill = openxlsx2::wb_color("FF3F3F7F"),
                                 font_color = openxlsx2::wb_color("FFFFFFFF")) |>
    openxlsx2::wb_add_dxfs_style(name = "long_day", bg_fill = openxlsx2::wb_color("FFFFE08A")) |>
    openxlsx2::wb_add_dxfs_style(name = "leave", bg_fill = openxlsx2::wb_color("FFB7E1CD")) |>
    add_sheet("grid", grid) |>
    openxlsx2::wb_freeze_pane("grid", first_active_row = 2, first_active_col = 3) |>
    highlight("_N", "night") |>
    highlight("_LD", "long_day")
  for (code in leave_codes) {
    wb <- highlight(wb, code, "leave")
  }
  wb <- add_sheet(wb, "long", long)

  openxlsx2::wb_save(wb, path, overwrite = TRUE)
  invisible(path)
}
