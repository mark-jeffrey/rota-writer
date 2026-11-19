# The fair_shares sheet (FAIR_SHARES), the cover sheet's pa column and the
# staff sheet's weekend_one_in. 1 Feb 2027 is a Monday.

# PA values: 1 PA is 4 h in weekday hours and 3 h out of hours. A night
# counts 17:00-02:00: 2 h in hours and 7 h out of hours on a weekday.
with_fair_shares <- function(inputs = consultant_inputs()) {
  inputs$cover$pa <- dplyr::case_when(
    inputs$cover$shift_code == "NIGHT" & inputs$cover$day_type == "weekday" ~ 2 / 4 + 7 / 3,
    inputs$cover$shift_code == "NIGHT" ~ 9 / 3,
    inputs$cover$day_type == "weekday" ~ 10 / 4,
    .default = 10 / 3
  )
  inputs$fair_shares <- tibble::tribble(
    ~share_id,   ~shifts,    ~day_types,     ~measure, ~weight,
    "FIRST",     "1ST",      NA,             "count",  10,
    "WE_DAYS",   "1ST;2ND",  "weekend",      "count",  10,
    "WE_NIGHTS", "NIGHT",    "weekend",      "count",  10,
    "BH",        NA,         "bank_holiday", "count",  10,
    "PAS",       NA,         NA,             "pa",     5
  )
  inputs$rules <- dplyr::bind_rows(
    inputs$rules,
    tibble::tibble(rule_id = "FAIR_SHARES", hard_soft = "soft", weight = 1, enabled = TRUE)
  )
  inputs
}

test_that("validate_inputs reports bad fair_shares rows, missing PAs and weekend limits", {
  inputs <- with_fair_shares()
  inputs$fair_shares$shifts[1] <- "3RD"
  inputs$fair_shares$day_types[2] <- "weekend;holiday"
  inputs$fair_shares$measure[3] <- "hours"
  inputs$fair_shares$share_id[4] <- "FIRST"
  inputs$staff$weekend_one_in[1] <- 0

  problems <- validate_inputs(inputs)$problem
  expect_true(any(grepl("Unknown shift_code: 3RD", problems)))
  expect_true(any(grepl("Unknown day type: holiday", problems)))
  expect_true(any(grepl("Unknown measure: hours", problems)))
  expect_true(any(grepl("FIRST is listed more than once", problems)))
  expect_true(any(grepl("weekend_one_in for C01", problems)))

  inputs <- with_fair_shares()
  inputs$fair_shares$share_id[1] <- "NIGHT"
  inputs$fair_shares$share_id[2] <- "Weekends"
  inputs$cover$pa[inputs$cover$shift_code == "2ND" & inputs$cover$day_type == "bank_holiday"] <- NA
  problems <- validate_inputs(inputs)$problem
  expect_true(any(grepl("NIGHT: pick another share_id", problems)))
  expect_true(any(grepl("Weekends: pick another share_id", problems)))
  expect_true(any(grepl("no pa value for 2ND on a bank_holiday", problems)))

  expect_equal(nrow(validate_inputs(with_fair_shares())), 0)
})

test_that("fair shares count shifts and PAs by slot and day type", {
  inputs <- with_fair_shares()
  inputs$bank_holidays <- tibble::tibble(date = as.Date("2027-02-03"), name = "Test holiday")
  rota <- dplyr::bind_rows(
    rota_of("C02", "2027-02-01", "24H"),                 # weekday: 2nd on + night
    rota_of("C01", c("2027-02-06", "2027-02-07"), "1ST"), # weekend 1st on
    rota_of("C03", "2027-02-06", "NIGHT"),               # weekend night
    rota_of("C04", "2027-02-03", "2ND")                  # bank holiday
  )
  d <- summarise_doctors(rota, inputs)
  row <- \(id) d[d$doctor_id == id, ]

  expect_equal(row("C02")$PAS, round(10 / 4 + 2 / 4 + 7 / 3, 2))
  expect_equal(row("C01")$PAS, round(2 * 10 / 3, 2))
  expect_equal(row("C03")$PAS, 3)
  expect_equal(row("C04")$PAS, round(10 / 3, 2))
  expect_equal(c(row("C01")$FIRST, row("C01")$WE_DAYS, row("C01")$BH), c(2, 2, 0))
  expect_equal(c(row("C03")$WE_NIGHTS, row("C03")$WE_DAYS), c(1, 0))
  expect_equal(row("C04")$BH, 1)
  expect_equal(row("C02")$BH, 0)
  # The 24-hour shift fills two slots, so it is two on-calls
  expect_equal(row("C02")$on_calls, 2)
  expect_equal(row("C01")$on_calls, 2)
  expect_equal(sum(d$fair_on_calls), sum(d$on_calls), tolerance = 0.5)
  # Shares follow WTE: C01 is full time, C02 three quarters
  expect_equal(sum(d$fair_FIRST), 2, tolerance = 0.5)
  expect_equal(row("C01")$fair_PAS / row("C02")$fair_PAS, 1 / 0.75, tolerance = 0.05)
})

test_that("an empty fair_shares sheet leaves the model as it was", {
  inputs <- consultant_inputs()
  before <- build_rota_model(inputs)
  inputs$rules <- dplyr::bind_rows(
    inputs$rules,
    tibble::tibble(rule_id = "FAIR_SHARES", hard_soft = "soft", weight = 1, enabled = TRUE)
  )
  after <- build_rota_model(inputs)
  expect_equal(after$rows, before$rows)
  expect_equal(after$costs, before$costs)
  expect_false("FAIR_SHARES" %in% after$costs$rule)
  rota <- rota_of("C01", "2027-02-01", "1ST")
  expect_equal(names(summarise_doctors(rota, inputs)), names(summarise_doctors(rota, consultant_inputs())))
})

test_that("the solver shares PAs and weekend days in proportion to WTE", {
  inputs <- with_fair_shares(consultant_inputs(weeks = 2))
  model <- build_rota_model(inputs)
  expect_true("FAIR_SHARES" %in% model$costs$rule)
  expect_true(any(grepl("^FAIR_SHARES total .*PAS$", model$rows$con)))

  solution <- solve_rota(inputs, time_limit = 30)
  expect_equal(solution$status, "Feasible")
  expect_true("FAIR_SHARES" %in% solution$penalties$rule)
  d <- summarise_doctors(solution$rota, inputs)
  # Two ordinary weeks: 5 x (2.5 + 2.5 + 2.83) + 2 x (3.33 + 3.33 + 3) = 117 PA
  expect_equal(sum(d$PAS), 117, tolerance = 0.01)
  expect_equal(sum(d$fair_PAS), 117, tolerance = 0.5)
  expect_lt(max(abs(d$PAS - d$fair_PAS)), 6)
})

test_that("a doctor's own weekend_one_in replaces the weekend limit in the checker and the model", {
  inputs <- consultant_inputs(weeks = 3)
  inputs$rules$enabled[inputs$rules$rule_id == "WEEKEND_MAX"] <- TRUE
  inputs$staff$weekend_one_in <- NA_real_
  inputs$staff$weekend_one_in[inputs$staff$doctor_id == "C01"] <- 1
  weekends <- as.Date(c("2027-02-06", "2027-02-13"))
  rota <- rota_of(rep(c("C01", "C03"), each = 2), rep(weekends, 2), "2ND")

  v <- hits(rota, inputs)
  expect_equal(v$doctor_id[v$rule_id == "WEEKEND_MAX"], "C03")
  expect_match(v$detail[v$rule_id == "WEEKEND_MAX"], "more than 1 in 3")

  model <- build_rota_model(inputs)
  limit <- \(id) model$rows$hi[model$rows$rule == "WEEKEND_MAX" & model$rows$con == id]
  expect_equal(limit("C01"), 3)
  expect_equal(limit("C03"), 1)
})
