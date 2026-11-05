# Short horizons keep these fast. Hard-rule tests switch the soft rules off,
# so the solver stops at the first compliant rota.

hard_only <- function(inputs) {
  inputs$rules$enabled[inputs$rules$hard_soft == "soft"] <- FALSE
  inputs
}

test_that("the solved rota passes every hard rule in the checker", {
  inputs <- two_weeks()
  solution <- solve_rota(inputs, time_limit = 20)

  expect_false(is.null(solution$rota))
  expect_equal(solution$status, "Feasible")
  expect_equal(nrow(solution$unfilled), 0)
  v <- check_rota(solution$rota, inputs)
  expect_equal(nrow(v[v$hard_soft == "hard", ]), 0)
})

test_that("the solver still passes when contract limits are tightened", {
  inputs <- hard_only(two_weeks())
  tighten <- c(MAX_CONSEC_NIGHTS = 3, MAX_7DAY = 62, MAX_CONSEC_LONG = 3, REST_AFTER_NIGHTS = 50)
  for (id in names(tighten)) {
    tight <- inputs
    tight$rules$value[tight$rules$rule_id == id] <- tighten[[id]]
    solution <- solve_rota(tight, time_limit = 30)

    expect(!is.null(solution$rota), paste("No rota with tightened", id))
    v <- check_rota(solution$rota, tight)
    expect(sum(v$hard_soft == "hard") == 0, paste("Hard violations with tightened", id))
  }
})

test_that("approved leave and nights-exempt doctors are respected", {
  inputs <- two_weeks()
  inputs$leave <- tibble::tibble(doctor_id = "D01", start_date = as.Date("2027-02-03"),
                                 end_date = as.Date("2027-02-05"), leave_type = "annual",
                                 status = "approved", notes = NA)
  x <- build_rota_model(inputs)$vars |> dplyr::filter(kind == "x")

  d01 <- x[x$doctor_id == "D01", ]
  expect_false(any(d01$date %in% (as.Date("2027-02-03") + 0:2)))
  # No night the evening before leave starts
  expect_false(any(d01$date == as.Date("2027-02-02") & d01$is_night))
  expect_false(any(x$doctor_id == "D11" & x$is_night))
})

test_that("ICU1 comes from anaesthetics, ICU2 from the tier 2 pool, and each pool has its own share", {
  inputs <- two_weeks()
  model <- build_rota_model(inputs)
  x <- dplyr::filter(model$vars, kind == "x")
  pool <- setNames(inputs$staff$pool, inputs$staff$doctor_id)

  expect_equal(unique(pool[x$doctor_id[x$shift_code %in% c("ICU2_LD", "ICU2_N")]]), "icu_tier2")
  expect_equal(unique(pool[x$doctor_id[x$shift_code %in% c("ICU1_LD", "ICU1_N")]]), "anaesthetics")
  expect_setequal(unique(pool[x$doctor_id[x$shift_code == "ICU_DAY"]]), c("anaesthetics", "icu_tier2"))
  # Saturday 6 Feb needs one ICU short day
  expect_true(any(x$shift_code == "ICU_DAY" & x$date == as.Date("2027-02-06")))
  expect_setequal(model$rows$con[model$rows$con %in% paste("FAIR_ONCALL total", pool)],
                  paste("FAIR_ONCALL total", c("anaesthetics", "icu_tier2")))
})

test_that("progress is reported, and returning TRUE stops the search with a compliant rota", {
  inputs <- two_weeks()
  calls <- list()
  solution <- solve_rota(inputs, time_limit = 120, progress = \(seconds, objective) {
    calls[[length(calls) + 1]] <<- c(seconds = seconds, objective = objective)
    length(calls) >= 3
  })

  expect_length(calls, 3)
  objective <- vapply(calls, \(x) x[["objective"]], numeric(1))
  expect_true(all(diff(objective) <= 0))
  expect_lt(solution$seconds, 120)
  expect_equal(solution$status, "Feasible")
  v <- check_rota(solution$rota, inputs)
  expect_equal(sum(v$hard_soft == "hard"), 0)
})

test_that("slots that cannot be filled are left empty and reported", {
  inputs <- hard_only(two_weeks())
  inputs$staff$can_OBS <- FALSE
  solution <- solve_rota(inputs, time_limit = 30)

  expect_equal(solution$status, "Unfilled shifts")
  expect_setequal(unique(solution$unfilled$shift_code), c("OBS_LD", "OBS_N"))
  expect_equal(nrow(solution$unfilled), 28) # 2 OBS shifts x 14 days
  # Every other slot is filled, and no contract rule is broken to do it
  v <- check_rota(solution$rota, inputs)
  expect_equal(unique(v$rule_id[v$hard_soft == "hard"]), "COVER")
  expect_equal(sum(v$rule_id == "COVER"), 28)
})

test_that("the solver's soft penalties match the checker's", {
  inputs <- two_weeks()
  solution <- solve_rota(inputs, time_limit = 20)

  checked <- check_rota(solution$rota, inputs) |>
    dplyr::summarise(penalty = sum(penalty), .by = c(hard_soft, rule_id)) |>
    dplyr::rename(rule = rule_id) |>
    dplyr::arrange(rule)
  solved <- solution$penalties |>
    dplyr::filter(!startsWith(rule, "FAIR_"), penalty != 0) |>
    dplyr::arrange(rule)
  expect_true(all(checked$hard_soft == "soft"))
  expect_equal(solved$rule, checked$rule)
  expect_equal(solved$penalty, checked$penalty)
  expect_equal(sum(solution$penalties$penalty), solution$objective, tolerance = 1e-6)
})

test_that("the model scores a fixed rota exactly as the checker does", {
  inputs <- two_weeks(test_inputs())
  inputs$rules$enabled[inputs$rules$rule_id == "LONG_NIGHT_RUN"] <- TRUE
  # D01 (ICU block): 6 shifts in a row, a split weekend (Sat 6 only), a
  # single day off (Sun 7), a 2-day ICU run (Mon 8 - Tue 9), another single
  # day off (Wed 10) and a single night (Thu 11). D06: 3 nights in a row.
  rota <- dplyr::bind_rows(
    rota_of("D01", days_from("2027-02-01", 5), "ICU_DAY"),
    rota_of("D01", c("2027-02-06", "2027-02-08", "2027-02-09", "2027-02-11"),
            c("ICU1_LD", "ICU_DAY", "ICU_DAY", "ICU1_N")),
    rota_of("D06", days_from("2027-02-01", 3), "ANA1_N")
  )
  checked <- check_rota(rota, inputs) |>
    dplyr::filter(hard_soft == "soft") |>
    dplyr::summarise(penalty = sum(penalty), .by = rule_id) |>
    dplyr::arrange(rule_id)
  expect_true(all(c("LONG_RUN", "SPLIT_WEEKEND", "ISOLATED_DAY_OFF", "ICU_SHORT_RUN",
                    "SINGLE_NIGHT", "LONG_NIGHT_RUN") %in% checked$rule_id))
  expect_equal(nrow(dplyr::filter(check_rota(rota, inputs), hard_soft == "hard", rule_id != "COVER")), 0)

  modelled <- model_penalties(rota, inputs)
  expect_equal(modelled$rule, checked$rule_id)
  expect_equal(modelled$penalty, checked$penalty)
})

test_that("requests and requested leave are honoured when the hard rules allow", {
  inputs <- hard_only(two_weeks())
  inputs$rules$enabled[inputs$rules$rule_id %in% c("REQUESTS", "LEAVE_REQUESTED")] <- TRUE
  inputs$leave <- tibble::tibble(doctor_id = "D01", start_date = as.Date("2027-02-01"),
                                 end_date = as.Date("2027-02-07"), leave_type = "annual",
                                 status = "requested", notes = NA)
  inputs$requests <- tibble::tibble(
    doctor_id = c("D02", "D06", "D06", "D12"),
    date = as.Date(c("2027-02-06", "2027-02-03", "2027-02-04", "2027-02-10")),
    request = c("no_weekend", "prefer_oncall", "no_night", "no_oncall"),
    weight = 10
  )
  solution <- solve_rota(inputs, time_limit = 20)

  expect_equal(nrow(check_rota(solution$rota, inputs)), 0)
  expect_equal(solution$objective, 0)
})

test_that("a failed final tidy-up keeps the rota found and prices it from its costs", {
  model <- list(vars = tibble::tibble(cost = c(2, 3, 0)), offset = tibble::tibble(value = 10))
  failing <- function(...) list(status_message = "Time limit reached",
                                info = list(primal_solution_status = "None"))
  current <- c(1, 1, 0)

  expect_warning(kept <- polish(failing, model, current), "rota itself is unchanged")
  expect_equal(kept$primal_solution, current)
  expect_equal(kept$objective_value, 15)
  expect_true(is_feasible(kept))
})

test_that("with no time limit anywhere, the solve stops after the default", {
  inputs <- hard_only(two_weeks())
  inputs$settings <- inputs$settings[inputs$settings$key != "solver_time_limit_s", ]
  expect_message(solution <- solve_rota(inputs), "stopping after 10 minutes")
  expect_equal(solution$status, "Feasible")
})

test_that("solver_workers defaults to 1 and ignores what it cannot read", {
  inputs <- two_weeks()
  expect_equal(solver_setting(inputs, "solver_workers"), 1)
  inputs$settings <- tibble::add_row(inputs$settings, key = c("solver_workers", "solver_other"),
                                     value = c("4", "zero"))
  expect_equal(solver_setting(inputs, "solver_workers"), 4)
  expect_equal(solver_setting(inputs, "solver_other"), 1)
})

test_that("a round's neighbourhoods free no shift in common", {
  model <- list(vars = tidyr::crossing(doctor_id = sprintf("D%02d", 1:12), t = 1:56) |>
                  dplyr::mutate(kind = "x", date = as.Date("2027-02-01") + t - 1))
  search <- list(days = 7, doctors = 5, whole_weeks = FALSE)
  set.seed(1)
  for (round in 1:2) {
    frees <- round_neighbourhoods(model, round, search, k = 4)
    expect_true(length(frees) >= 2)
    expect_true(all(Reduce(`+`, frees) <= 1))
  }
  # Only two groups of 5 fit in 12 doctors
  expect_length(round_neighbourhoods(model, 2, search, k = 4), 2)
})

test_that("improvements to different parts are combined only if the rota stays valid", {
  model <- list(vars = tibble::tibble(kind = "x", cost = c(2, 0, 1, 0)))
  best <- list(primal_solution = c(0, 0, 0, 0), objective_value = 10,
               info = list(primal_solution_status = "Feasible"))
  found <- \(x, obj) list(primal_solution = x, objective_value = obj,
                          info = list(primal_solution_status = "Feasible"))
  tried <- list(found(c(1, 0, 0, 0), 8), found(c(0, 0, 1, 0), 9))
  frees <- list(c(TRUE, TRUE, FALSE, FALSE), c(FALSE, FALSE, TRUE, TRUE))
  # A stand-in for the fixed-rota check: penalty 10 less 2 per x1 and 1 per x3,
  # and a "hard rule" allowing at most `cap` shifts in all
  checker <- \(cap) function(cost, time_limit, fix, current) {
    if (sum(current) > cap) list(info = list(primal_solution_status = "Infeasible"))
    else found(current, 10 - 2 * current[1] - current[3])
  }

  both <- combine_improvements(checker(2), model, best, tried, frees)
  expect_equal(both$primal_solution, c(1, 0, 1, 0))
  expect_equal(both$objective_value, 7)

  # Together they break the hard rule, so only the better one is kept
  one <- combine_improvements(checker(1), model, best, tried, frees)
  expect_equal(one$primal_solution, c(1, 0, 0, 0))

  # Overlapping parts are never combined
  overlap <- combine_improvements(checker(2), model, best, tried, list(frees[[1]], c(TRUE, FALSE, TRUE, TRUE)))
  expect_equal(overlap$objective_value, 8)
})

test_that("a solve on several cores is compliant", {
  skip_on_cran()
  inputs <- two_weeks()
  solution <- solve_rota(inputs, time_limit = 30, workers = 2)
  expect_equal(solution$status, "Feasible")
  v <- check_rota(solution$rota, inputs)
  expect_equal(sum(v$hard_soft == "hard"), 0)
})
