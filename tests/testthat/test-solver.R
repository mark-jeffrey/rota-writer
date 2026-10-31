# Short horizons keep these fast; the full 13 weeks solves in under 10 s.

two_weeks <- function(inputs = read_rota_inputs(system.file("extdata", "rota_inputs.xlsx",
                                                            package = "rotawriter"))) {
  inputs$period$end <- inputs$period$start + 13
  inputs
}

test_that("the solved rota passes every hard rule in the checker", {
  inputs <- two_weeks()
  solution <- solve_rota(inputs, time_limit = 60)

  expect_false(is.null(solution$rota))
  v <- check_rota(solution$rota, inputs)
  expect_equal(nrow(v[v$hard_soft == "hard", ]), 0)
})

test_that("the solver still passes when contract limits are tightened", {
  inputs <- two_weeks()
  tighten <- c(MAX_CONSEC_NIGHTS = 3, MAX_7DAY = 62, MAX_CONSEC_LONG = 3, REST_AFTER_NIGHTS = 50)
  for (id in names(tighten)) {
    tight <- inputs
    tight$rules$value[tight$rules$rule_id == id] <- tighten[[id]]
    solution <- solve_rota(tight, time_limit = 60)

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
  rota <- solve_rota(inputs, time_limit = 60)$rota
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_rota(rota, inputs, path)

  back <- read_rota(path)
  expect_equal(dplyr::semi_join(back, inputs$shifts, by = c(code = "shift_code")) |>
                 dplyr::arrange(date, doctor_id),
               rota)
  expect_equal(readxl::excel_sheets(path), c("grid", "long"))
})
