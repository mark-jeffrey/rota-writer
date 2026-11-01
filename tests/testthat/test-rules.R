# One rule per block. D01: ST8, full time, ICU block all rota. 2027-02-01 is a Monday.

test_that("a sensible single-doctor pattern breaks no doctor-level hard rule", {
  rota <- rota_of(
    "D01",
    c(days_from("2027-02-01", 4), as.Date("2027-02-08") + 0:4, as.Date("2027-02-13")),
    c(rep("ICU1_N", 4), rep("ICU_DAY", 5), "ICU1_LD")
  )
  v <- hits(rota)
  expect_equal(nrow(v[v$hard_soft == "hard", ]), 0)
})

test_that("MIN_REST catches short gaps and overlaps", {
  v <- hits(rota_of("D01", c("2027-02-01", "2027-02-01"), c("ICU_DAY", "ICU1_N")))
  expect_match(v$detail[v$rule_id == "MIN_REST"], "3.0 h rest")

  v <- hits(rota_of("D01", c("2027-02-01", "2027-02-02"), c("ICU1_N", "ICU1_LD")))
  expect_match(v$detail[v$rule_id == "MIN_REST"], "overlaps")
})

test_that("night rules: max 4 in a row, 46 h rest after, single nights soft", {
  v <- hits(rota_of("D01", days_from("2027-02-01", 5), "ICU1_N"))
  expect_true("MAX_CONSEC_NIGHTS" %in% v$rule_id)

  v <- hits(rota_of("D01", days_from("2027-02-01", 4), "ICU1_N"))
  expect_false("MAX_CONSEC_NIGHTS" %in% v$rule_id)

  # Night ends Tue 08:30, next night Wed 20:00: 35.5 h rest
  v <- hits(rota_of("D01", c("2027-02-01", "2027-02-03"), "ICU1_N"))
  expect_true("REST_AFTER_NIGHTS" %in% v$rule_id)
  expect_false("MIN_REST" %in% v$rule_id)
  expect_equal(sum(v$rule_id == "SINGLE_NIGHT"), 2)
  expect_true(all(v$hard_soft[v$rule_id == "SINGLE_NIGHT"] == "soft"))

  # Four nights then a long day 47.5 h after the last one ends
  v <- hits(rota_of("D01", c(days_from("2027-02-01", 4), as.Date("2027-02-07")),
                    c(rep("ICU1_N", 4), "ICU1_LD")))
  expect_false("REST_AFTER_NIGHTS" %in% v$rule_id)
})

test_that("runs beyond the LONG_RUN and LONG_NIGHT_RUN targets cost per extra day", {
  v <- hits(rota_of("D01", days_from("2027-02-01", 7), "ICU_DAY"))
  expect_equal(v$penalty[v$rule_id == "LONG_RUN"], 2 * 150)
  expect_match(v$detail[v$rule_id == "LONG_RUN"], "7 consecutive shifts \\(target 5\\)")

  v <- hits(rota_of("D01", days_from("2027-02-01", 5), "ICU_DAY"))
  expect_false("LONG_RUN" %in% v$rule_id)

  # Off in the test workbook
  inputs <- test_inputs()
  inputs$rules$enabled[inputs$rules$rule_id == "LONG_NIGHT_RUN"] <- TRUE
  v <- hits(rota_of("D01", days_from("2027-02-01", 4), "ICU1_N"), inputs)
  expect_equal(v$penalty[v$rule_id == "LONG_NIGHT_RUN"], 2 * 50)

  v <- hits(rota_of("D01", days_from("2027-02-01", 2), "ICU1_N"), inputs)
  expect_false(any(c("LONG_NIGHT_RUN", "SINGLE_NIGHT") %in% v$rule_id))
})

test_that("wellbeing rules: short ICU runs, single days off, split weekends", {
  # Wed only: a 1-day ICU run costs 2 x 5; Tue-Thu is long enough
  v <- hits(rota_of("D01", "2027-02-03", "ICU1_LD"))
  expect_equal(v$penalty[v$rule_id == "ICU_SHORT_RUN"], 2 * 5)
  v <- hits(rota_of("D01", days_from("2027-02-02", 3), "ICU_DAY"))
  expect_false("ICU_SHORT_RUN" %in% v$rule_id)

  v <- hits(rota_of("D01", c("2027-02-01", "2027-02-03", "2027-02-04", "2027-02-05"), "ICU_DAY"))
  expect_equal(v$start_date[v$rule_id == "ISOLATED_DAY_OFF"], as.Date("2027-02-02"))

  # Sat 6 Feb only; a Friday night alone ends on Saturday morning
  expect_true("SPLIT_WEEKEND" %in% hits(rota_of("D01", "2027-02-06", "ICU1_LD"))$rule_id)
  expect_true("SPLIT_WEEKEND" %in% hits(rota_of("D01", "2027-02-05", "ICU1_N"))$rule_id)
  v <- hits(rota_of("D01", c("2027-02-05", "2027-02-06"), "ICU1_N"))
  expect_false("SPLIT_WEEKEND" %in% v$rule_id)
})

test_that("MAX_7DAY counts hours in any 168 h", {
  v <- hits(rota_of("D01", days_from("2027-02-01", 6), "ICU1_LD"))
  expect_match(v$detail[v$rule_id == "MAX_7DAY"], "75.0 h")

  # 4 long days + 1 normal day = 59 h
  v <- hits(rota_of("D01", days_from("2027-02-01", 5), c(rep("ICU1_LD", 4), "ICU_DAY")))
  expect_false("MAX_7DAY" %in% v$rule_id)
})

test_that("consecutive shift and long shift limits, with 48 h rest", {
  v <- hits(rota_of("D01", days_from("2027-02-01", 8), "ICU_DAY"))
  expect_true("MAX_CONSEC_SHIFTS" %in% v$rule_id)

  # 7 days, then back 39 h later
  v <- hits(rota_of("D01", c(days_from("2027-02-01", 7), as.Date("2027-02-09")), "ICU_DAY"))
  expect_match(v$detail[v$rule_id == "REST_AFTER_RUN"], "after 7 consecutive shifts")

  v <- hits(rota_of("D01", c(days_from("2027-02-01", 7), as.Date("2027-02-10")), "ICU_DAY"))
  expect_false(any(c("MAX_CONSEC_SHIFTS", "REST_AFTER_RUN") %in% v$rule_id))

  v <- hits(rota_of("D01", days_from("2027-02-01", 5), "ICU1_LD"))
  expect_true("MAX_CONSEC_LONG" %in% v$rule_id)

  # Nights are not counted as long shifts for this rule
  v <- hits(rota_of("D01", days_from("2027-02-01", 4), "ICU1_N"))
  expect_false("MAX_CONSEC_LONG" %in% v$rule_id)
})

test_that("weekends include Friday and Sunday nights but not Monday", {
  si <- shift_instances(rota_of("D01", c("2027-02-05", "2027-02-14", "2027-02-15"),
                                c("ICU1_N", "ICU1_N", "ICU_DAY")), test_inputs())
  expect_equal(weekends_worked(si)$sat, as.Date(c("2027-02-06", "2027-02-13")))
})

test_that("weekend frequency: more than 1 in 2 is hard, more than 1 in 3 is soft", {
  saturdays <- as.Date("2027-02-06") + 7 * 0:12

  v <- hits(rota_of("D01", saturdays[c(TRUE, FALSE)], "ICU1_LD")) # 7 of 13
  expect_true("WEEKEND_MAX" %in% v$rule_id)

  v <- hits(rota_of("D01", saturdays[1:6], "ICU1_LD")) # 6 of 13
  expect_false("WEEKEND_MAX" %in% v$rule_id)
  expect_equal(v$penalty[v$rule_id == "WEEKEND_TARGET"], 2 * 50)
})

test_that("average hours are pro rata to WTE", {
  weekdays <- days_from("2027-02-01", 91)
  weekdays <- weekdays[lubridate::wday(weekdays, week_start = 1) <= 5]

  # D01 full time: 5 x 9 h = 45 h/week is fine
  v <- hits(rota_of("D01", weekdays, "ICU_DAY"))
  expect_false("AVG_HOURS" %in% v$rule_id)

  # D05 at 0.6 WTE, Wednesdays off: 4 x 9 h = 36 h/week > 28.8
  not_wed <- weekdays[lubridate::wday(weekdays, week_start = 1) != 3]
  v <- hits(rota_of("D05", not_wed, "ANA_DAY"))
  expect_match(v$detail[v$rule_id == "AVG_HOURS"], "max 28.8")
  expect_false("LTFT_FIXED_DAY" %in% v$rule_id)
})

test_that("LTFT fixed day, nights exemption, competency and in-post", {
  expect_true("LTFT_FIXED_DAY" %in% hits(rota_of("D05", "2027-02-03", "ANA_DAY"))$rule_id)
  expect_true("NIGHTS_EXEMPT" %in% hits(rota_of("D11", "2027-02-01", "ANA1_N"))$rule_id)
  expect_true("COMPETENCY" %in% hits(rota_of("D24", "2027-02-01", "OBS_N"))$rule_id)
  expect_true("IN_POST" %in% hits(rota_of("D34", "2027-02-01", "ICU2_N"))$rule_id)
  expect_false("IN_POST" %in% hits(rota_of("D34", "2027-03-03", "ICU2_N"))$rule_id)
})

test_that("placement rules: forbidden roles are hard, discouraged roles are penalised", {
  expect_true("PLACEMENT" %in% hits(rota_of("D01", "2027-02-01", "ANA1_N"))$rule_id)
  expect_true("PLACEMENT" %in% hits(rota_of("D07", "2027-02-01", "ANA2_N"))$rule_id)

  v <- hits(rota_of("D02", "2027-02-01", "OBS_N"))
  expect_equal(v$penalty[v$rule_id == "PLACEMENT_AVOID"], 100)
  expect_false("PLACEMENT" %in% v$rule_id)
})

test_that("approved leave is hard and requested leave is soft", {
  inputs <- test_inputs()
  inputs$leave <- tibble::tibble(
    doctor_id = "D01", start_date = as.Date(c("2027-02-01", "2027-02-08")),
    end_date = as.Date(c("2027-02-02", "2027-02-08")), leave_type = "annual",
    status = c("approved", "requested")
  )
  v <- hits(rota_of("D01", c("2027-02-02", "2027-02-08"), "ICU_DAY"), inputs)

  expect_equal(v$hard_soft[v$rule_id == "LEAVE_APPROVED"], "hard")
  expect_equal(v$penalty[v$rule_id == "LEAVE_REQUESTED"], 200)
})

test_that("a night may not finish on a leave day, but a long day the day before is fine", {
  inputs <- test_inputs()
  inputs$leave <- tibble::tibble(
    doctor_id = "D01", start_date = as.Date("2027-02-02"), end_date = as.Date("2027-02-05"),
    leave_type = "annual", status = "approved"
  )

  v <- hits(rota_of("D01", "2027-02-01", "ICU1_N"), inputs)
  expect_match(v$detail[v$rule_id == "LEAVE_APPROVED"], "finishes on approved annual leave")
  expect_equal(v$end_date[v$rule_id == "LEAVE_APPROVED"], as.Date("2027-02-02"))

  v <- hits(rota_of("D01", "2027-02-01", "ICU1_LD"), inputs)
  expect_false("LEAVE_APPROVED" %in% v$rule_id)

  # A night starting on the last leave day still clashes, once
  v <- hits(rota_of("D01", "2027-02-05", "ICU1_N"), inputs)
  expect_equal(sum(v$rule_id == "LEAVE_APPROVED"), 1)
  expect_match(v$detail[v$rule_id == "LEAVE_APPROVED"], " - on approved")
})

test_that("requests are penalised by their own weight", {
  inputs <- test_inputs()
  inputs$requests <- tibble::tibble(
    doctor_id = "D01", date = as.Date(c("2027-02-01", "2027-02-02", "2027-02-13")),
    request = c("no_oncall", "prefer_oncall", "no_weekend"), weight = c(5, 2, 3)
  )
  v <- hits(rota_of("D01", c("2027-02-01", "2027-02-02", "2027-02-12"),
                    c("ICU1_LD", "ICU_DAY", "ICU1_N")), inputs)

  expect_equal(sort(v$penalty[v$rule_id == "REQUESTS"]), c(2, 3, 5))
})

test_that("skill mix flags nights without an ST5+ in a first on-call tier", {
  rota <- rota_of(c("D12", "D13", "D14"), "2027-02-01", c("ICU1_N", "ANA1_N", "OBS_N"))
  v <- check_rota(rota, test_inputs())
  sm1 <- v[v$rule_id == "SKILL_MIX" & grepl("^SM1", v$detail) & v$start_date == as.Date("2027-02-01"), ]
  expect_equal(nrow(sm1), 1)
  expect_equal(sm1$penalty, 1000)

  rota$doctor_id[1] <- "D01"
  v <- check_rota(rota, test_inputs())
  expect_false(any(v$rule_id == "SKILL_MIX" & grepl("^SM1", v$detail) &
                     v$start_date == as.Date("2027-02-01")))
})

test_that("disabled rules are not reported", {
  inputs <- test_inputs()
  inputs$rules$enabled[inputs$rules$rule_id == "MIN_REST"] <- FALSE
  v <- hits(rota_of("D01", c("2027-02-01", "2027-02-01"), c("ICU_DAY", "ICU1_N")), inputs)
  expect_false("MIN_REST" %in% v$rule_id)
})
