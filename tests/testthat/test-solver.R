# Short horizons keep these fast. Hard-rule tests switch the soft rules off,
# so the solver stops at the first compliant rota.

two_weeks <- function(inputs = read_rota_inputs(system.file("extdata", "rota_inputs.xlsx",
                                                            package = "rotawriter"))) {
  inputs$period$end <- inputs$period$start + 13
  inputs
}

hard_only <- function(inputs) {
  inputs$rules$enabled[inputs$rules$hard_soft == "soft"] <- FALSE
  inputs
}

test_that("the solved rota passes every hard rule in the checker", {
  inputs <- two_weeks()
  solution <- solve_rota(inputs, time_limit = 20)

  expect_false(is.null(solution$rota))
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

test_that("unfillable slots are reported before solving", {
  inputs <- two_weeks()
  inputs$staff$can_OBS <- FALSE

  expect_error(build_rota_model(inputs), "OBS_LD: 0 eligible, needs 1")
})

test_that("write_rota round-trips through read_rota", {
  inputs <- two_weeks()
  rota <- solve_rota(hard_only(inputs), time_limit = 30)$rota
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_rota(rota, inputs, path)

  back <- read_rota(path)
  expect_equal(dplyr::semi_join(back, inputs$shifts, by = c(code = "shift_code")) |>
                 dplyr::arrange(date, doctor_id),
               rota)
  expect_equal(readxl::excel_sheets(path), c("grid", "long"))
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
