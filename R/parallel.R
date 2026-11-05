# Phase 2 of solve_rota() on several cores: each round, `workers` R
# processes each re-solve a different part of the rota, all starting from the
# best rota so far. Improvements to parts that don't overlap are combined, and
# the combination is kept only if the whole rota still meets every hard rule.

# A whole-number setting of 1 or more from the settings sheet, else 1
solver_setting <- function(inputs, key) {
  value <- suppressWarnings(as.numeric(inputs$settings$value[match(key, inputs$settings$key)]))
  if (is.na(value) || value < 1) 1 else floor(value)
}

# At most one less than the computer's cores, so it stays usable
usable_workers <- function(workers) {
  cores <- parallel::detectCores()
  limit <- if (is.na(cores)) 1 else max(cores - 1, 1)
  if (workers > limit) {
    message("Using ", limit, " workers: this computer has ", cores, " cores")
  }
  min(workers, limit)
}

search_parallel <- function(model, solve, best, search, workers, time_left, report,
                            stop_now) {
  cluster <- start_workers(model, workers)
  on.exit(parallel::stopCluster(cluster), add = TRUE)

  round <- 0
  stalled <- 0
  step_time <- 5
  while (time_left() > 1 && best$objective_value > 1e-6) {
    round <- round + 1
    frees <- round_neighbourhoods(model, round, search, workers)
    limit <- min(step_time, time_left())
    # A function with no enclosing data, so only these arguments are sent
    step <- worker_step
    environment(step) <- globalenv()
    tried <- parallel::clusterApply(cluster, frees, step, limit, best$primal_solution)

    combined <- combine_improvements(solve, model, best, tried, frees)
    if (combined$objective_value < best$objective_value - 1e-6) {
      best <- combined
      stalled <- 0
      report(best$objective_value)
    } else {
      stalled <- stalled + length(frees)
      if (stalled >= 10) {
        stalled <- 0
        step_time <- min(2 * step_time, 20)
      }
    }
    if (stop_now(best$objective_value)) break
  }
  best
}

# Start the worker processes, each with its own copy of the model and solver
start_workers <- function(model, workers) {
  cluster <- parallel::makePSOCKcluster(workers)
  init <- worker_init
  # Run in the worker's global environment, not this package's namespace,
  # which a worker can only load once it knows where the package is
  environment(init) <- globalenv()
  tryCatch(
    parallel::clusterCall(cluster, init, dev_package_path(), model),
    error = \(e) {
      parallel::stopCluster(cluster)
      stop("Could not start the solver's worker processes: ", conditionMessage(e), call. = FALSE)
    }
  )
  cluster
}

worker_init <- function(dev_path, model) {
  if (is.null(dev_path)) {
    loadNamespace("rotawriter")
  } else {
    pkgload::load_all(dev_path, quiet = TRUE)
  }
  solver <- utils::getFromNamespace("highs_solver", "rotawriter")(model)
  assign(".rota_step", function(free, time_limit, current) {
    result <- solver(cost = model$vars$cost, time_limit = time_limit, fix = !free, current = current)
    # Only what the main process needs, to keep the reply small
    list(primal_solution = result$primal_solution, objective_value = result$objective_value,
         info = list(primal_solution_status = result$info$primal_solution_status))
  }, envir = globalenv())
  TRUE
}

# Runs in a worker; .rota_step is set up by worker_init()
worker_step <- function(free, time_limit, current) {
  get(".rota_step", envir = globalenv())(free, time_limit, current)
}

# Up to `k` neighbourhoods that free no shift in common: day windows on odd
# rounds and groups of doctors on even ones, as neighbourhood() does
round_neighbourhoods <- function(model, round, search, k) {
  is_x <- model$vars$kind == "x"
  t <- model$vars$t
  if (round %% 2 == 1) {
    starts <- if (isTRUE(search$whole_weeks)) {
      sort(unique(t[is_x & lubridate::wday(model$vars$date, week_start = 1) == 1]))
    } else {
      seq_len(max(max(t, na.rm = TRUE) - search$days + 1, 1))
    }
    chosen <- integer()
    for (first in starts[sample.int(length(starts))]) {
      if (all(abs(first - chosen) >= search$days)) chosen <- c(chosen, first)
      if (length(chosen) == k) break
    }
    map(chosen, \(first) is_x & t >= first & t < first + search$days)
  } else {
    ids <- sample(unique(model$vars$doctor_id[is_x]))
    size <- min(search$doctors, length(ids))
    groups <- max(min(k, length(ids) %/% size), 1)
    map(seq_len(groups), \(g) is_x & model$vars$doctor_id %in% ids[(g - 1) * size + seq_len(size)])
  }
}

# The best single improvement, plus any others that change different shifts
# and still leave a rota meeting every hard rule. Each addition is checked by
# solving with every shift fixed, as polish() does, which also prices it.
combine_improvements <- function(solve, model, best, tried, frees) {
  improved <- which(map_lgl(tried, \(r) is_feasible(r) &&
                              r$objective_value < best$objective_value - 1e-6))
  if (length(improved) == 0) {
    return(best)
  }
  improved <- improved[order(map_dbl(tried[improved], "objective_value"))]
  is_x <- model$vars$kind == "x"
  combined <- tried[[improved[1]]]
  changed <- frees[[improved[1]]]

  for (i in improved[-1]) {
    if (any(changed & frees[[i]])) next
    merged <- combined$primal_solution
    merged[is_x] <- round(merged[is_x] + tried[[i]]$primal_solution[is_x] - best$primal_solution[is_x])
    checked <- solve(cost = model$vars$cost, time_limit = 10, fix = rep(TRUE, nrow(model$vars)),
                     current = merged)
    if (is_feasible(checked) && checked$objective_value < combined$objective_value - 1e-6) {
      combined <- checked
      changed <- changed | frees[[i]]
    }
  }
  combined
}
