# The ICU consultant rota type: 24-hour shifts filling two slots, block
# patterns, standing preferences and shift sequences. 1 Feb 2027 is a
# Monday.

penalty_of <- function(v, rule) sum(v$penalty[v$rule_id == rule])

test_that("the consultant template reads and validates cleanly", {
  inputs <- read_rota_inputs(system.file("extdata", "icu_consultant_inputs.xlsx", package = "rotawriter"))

  expect_equal(rota_type(inputs$settings), "consultant")
  expect_equal(as.numeric(inputs$period$end - inputs$period$start + 1), 364)
  expect_equal(nrow(validate_inputs(inputs)), 0)
  # TCS rules are not in its rules sheet, so they are off
  expect_false(rule_on(inputs, "MAX_7DAY"))
  expect_true(rule_on(inputs, "PATTERNS"))
})

test_that("validate_inputs reports bad patterns, preferences, sequences and block rest", {
  inputs <- consultant_inputs()
  inputs$patterns$days[1] <- "Mon;Funday"
  inputs$patterns$slot[2] <- "3RD"
  inputs$preferences$value[1] <- "1ST_6D"
  inputs$preferences$preference[2] <- "prefers_lie_ins"
  inputs$sequences$to_shift[1] <- "NITE"
  inputs$shifts$covers[4] <- "2ND;NITE"
  inputs$block_rest$pattern_id[1] <- "1ST_8D"
  inputs$pattern_sequences$to_pattern[1] <- "1ST_TW"
  inputs$block_rest$shifts[2] <- "24H;36H"

  problems <- validate_inputs(inputs)$problem
  expect_true(any(grepl("1ST_MT: days must be", problems)))
  expect_true(any(grepl("Unknown slot: 3RD", problems)))
  expect_true(any(grepl("Unknown value for prefer_pattern: 1ST_6D", problems)))
  expect_true(any(grepl("Unknown preference: prefers_lie_ins", problems)))
  expect_true(any(grepl("sequences|Unknown shift_code: NITE", problems)))
  expect_true(any(grepl("Unknown shift_code in covers: NITE", problems)))
  expect_true(any(grepl("Unknown pattern_id: 1ST_8D", problems)))
  expect_true(any(grepl("Unknown pattern_id: 1ST_TW", problems)))
  expect_true(any(grepl("Unknown shift_code: 36H", problems)))
})

test_that("a 24-hour shift fills both the 2nd-on and night slots", {
  inputs <- consultant_inputs()
  v <- check_rota(rota_of(c("C01", "C02"), "2027-02-01", c("1ST", "24H")), inputs)

  expect_false(any(v$rule_id == "COVER" & v$start_date == as.Date("2027-02-01")))
  expect_equal(v$rule_id[v$rule_id == "COVER" & v$start_date == as.Date("2027-02-02")],
               rep("COVER", 3))
})

test_that("24-hour shifts and 7-day blocks are only for consultants who prefer them", {
  inputs <- consultant_inputs()
  expect_true("PREFERENCE_ONLY" %in% hits(rota_of("C01", "2027-02-01", "24H"), inputs)$rule_id)
  expect_false("PREFERENCE_ONLY" %in% hits(rota_of("C02", "2027-02-01", "24H"), inputs)$rule_id)

  vars <- build_rota_model(inputs)$vars
  expect_setequal(unique(vars$doctor_id[vars$kind == "x" & vars$shift_code == "24H"]),
                  c("C02", "C05", "C11", "C14"))
  seven_day <- vars$kind == "block" & grepl("1ST_7D", vars$aux)
  expect_setequal(unique(vars$doctor_id[seven_day]), c("C03", "C08"))
})

test_that("rest and sequence rules keep nights apart from day shifts", {
  inputs <- consultant_inputs()
  rule_ids <- \(doctor, dates, codes) hits(rota_of(doctor, dates, codes), inputs)$rule_id

  expect_true("MIN_REST" %in% rule_ids("C01", c("2027-02-01", "2027-02-02"), c("NIGHT", "2ND")))
  expect_true("MIN_REST" %in% rule_ids("C02", c("2027-02-01", "2027-02-02"), c("24H", "1ST")))
  expect_true("SEQUENCE" %in% rule_ids("C01", c("2027-02-01", "2027-02-02"), c("1ST", "NIGHT")))
  expect_false(any(c("MIN_REST", "SEQUENCE") %in%
                     rule_ids("C01", c("2027-02-01", "2027-02-03", "2027-02-05"), "NIGHT")))
})

test_that("days outside a block pattern are penalised, split into blocks the cheapest way", {
  inputs <- consultant_inputs()
  first_on <- \(doctor, from, n) hits(rota_of(doctor, days_from(from, n), "1ST"), inputs)

  expect_equal(penalty_of(first_on("C01", "2027-02-01", 4), "PATTERNS"), 0) # Mon-Thu
  expect_equal(penalty_of(first_on("C01", "2027-02-02", 3), "PATTERNS"), 300) # Tue-Thu
  expect_equal(penalty_of(first_on("C01", "2027-02-01", 7), "PATTERNS"), 0) # Mon-Thu + Fri-Sun
  # C03 prefers 7-day weeks: a whole week is one block, a Mon-Thu block costs
  expect_equal(penalty_of(first_on("C03", "2027-02-01", 7), "PREFERENCES"), 0)
  expect_equal(penalty_of(first_on("C03", "2027-02-01", 4), "PREFERENCES"), 50)

  # A whole week of 1st on for someone who has not asked for 7-day weeks
  expect_equal(penalty_of(first_on("C01", "2027-02-01", 7), "PATTERN_UNPREFERRED"), 100)
  expect_equal(penalty_of(first_on("C03", "2027-02-01", 7), "PATTERN_UNPREFERRED"), 0)

  nights <- \(doctor, dates) hits(rota_of(doctor, dates, "NIGHT"), inputs)
  mwfs <- as.Date("2027-02-01") + c(0, 2, 4, 6)
  expect_equal(penalty_of(nights("C01", mwfs), "PATTERNS"), 0)
  expect_equal(penalty_of(nights("C10", mwfs), "PREFERENCES"), 10) # avoids N_MWFS
  # A 24-hour shift can be one of a night block's nights
  v <- hits(rota_of("C02", mwfs, c("NIGHT", "24H", "NIGHT", "NIGHT")), inputs)
  expect_equal(penalty_of(v, "PATTERNS"), 0)
})

test_that("big blocks need rest before and after them, timed in clock hours", {
  inputs <- consultant_inputs(weeks = 3)
  week2 <- days_from("2027-02-08", 7)
  rest <- \(rota) hits(rota, inputs)[hits(rota, inputs)$rule_id == "BLOCK_REST", ]
  seven_days_and <- \(dates, codes) dplyr::bind_rows(rota_of("C03", week2, "1ST"),
                                                      rota_of("C03", dates, codes))

  # 2nd on the Saturday before, or a night starting 17:00 the Tuesday after
  expect_equal(nrow(rest(seven_days_and("2027-02-06", "2ND"))), 1)
  expect_equal(nrow(rest(seven_days_and("2027-02-16", "NIGHT"))), 1)
  expect_equal(rest(seven_days_and("2027-02-16", "NIGHT"))$penalty, 1000)
  # A Friday night ends 48 h before Monday 08:00; Wednesday is 48 h after Sunday 18:00
  expect_equal(nrow(rest(seven_days_and(c("2027-02-05", "2027-02-17"), c("NIGHT", "2ND")))), 0)
  # Six days of 1st on is not a 7-day block
  expect_equal(nrow(rest(dplyr::bind_rows(rota_of("C03", week2[-7], "1ST"),
                                          rota_of("C03", "2027-02-06", "2ND")))), 0)

  # A night block all as 24-hour shifts, but not one with an ordinary night in it
  mwfs <- as.Date("2027-02-08") + c(0, 2, 4, 6)
  expect_equal(nrow(rest(rota_of("C02", c(mwfs, as.Date("2027-02-16")), c(rep("24H", 4), "2ND")))), 1)
  expect_equal(nrow(rest(rota_of("C02", c("2027-02-06", "2027-02-09"), "2ND") |>
                           dplyr::bind_rows(rota_of("C02", mwfs[-2] + c(0, 1, 0), "24H")))), 0)
  expect_equal(nrow(rest(rota_of("C02", c(mwfs, as.Date("2027-02-16")),
                                 c("24H", "NIGHT", "24H", "24H", "2ND")))), 0)
})

test_that("nights in two weeks running are penalised once per pair of weeks", {
  inputs <- consultant_inputs(weeks = 3)
  pairs <- \(dates) {
    v <- hits(rota_of("C01", dates, "NIGHT"), inputs)
    v[v$rule_id == "CONSEC_NIGHT_WEEKS", ]
  }

  expect_equal(nrow(pairs(c("2027-02-07", "2027-02-09"))), 1) # Sunday, then Tuesday
  expect_equal(pairs(c("2027-02-07", "2027-02-09"))$penalty, 500)
  expect_equal(nrow(pairs(c("2027-02-02", "2027-02-09", "2027-02-16"))), 2)
  expect_equal(nrow(pairs(c("2027-02-02", "2027-02-16"))), 0)
  # A 24-hour shift counts as a night
  v <- hits(rota_of("C02", c("2027-02-07", "2027-02-09"), "24H"), inputs)
  expect_equal(sum(v$rule_id == "CONSEC_NIGHT_WEEKS"), 1)
})

test_that("1st on Fri-Sun then Mon-Thu the next week is penalised", {
  inputs <- consultant_inputs(weeks = 3)
  pairs <- \(dates) {
    v <- hits(rota_of("C01", dates, "1ST"), inputs)
    v[v$rule_id == "PATTERN_SEQUENCE", ]
  }

  expect_equal(pairs(days_from("2027-02-05", 7))$penalty, 500) # Fri 5 to Thu 11
  expect_equal(pairs(days_from("2027-02-05", 7))$end_date, as.Date("2027-02-11"))
  expect_equal(nrow(pairs(days_from("2027-02-05", 6))), 0)     # only Mon-Wed after
  expect_equal(nrow(pairs(days_from("2027-02-01", 7))), 0)     # Mon-Thu then Fri-Sun
})

test_that("the model scores block rest, night weeks and pattern sequences as the checker does", {
  inputs <- consultant_inputs(weeks = 5)
  rota <- dplyr::bind_rows(
    rota_of("C03", days_from("2027-02-08", 7), "1ST"),        # 7 days 1st on...
    rota_of("C03", c("2027-02-06", "2027-02-16"), c("2ND", "NIGHT")), # ...with a shift either side
    rota_of("C02", as.Date("2027-02-01") + c(0, 2, 4, 6), "24H"), # a week of 24-hour shifts...
    rota_of("C02", "2027-02-09", "2ND"),                       # ...and 2nd on the Tuesday after
    rota_of("C05", c("2027-02-02", "2027-02-04", "2027-02-06", "2027-02-09", "2027-02-15"),
            "NIGHT"),                                          # nights in three weeks running
    rota_of("C08", "2027-02-12", "NIGHT"),                     # 48 h before 7 days 1st on
    rota_of("C08", days_from("2027-02-15", 7), "1ST"),
    rota_of("C01", days_from("2027-02-26", 7), "1ST")          # Fri-Sun then Mon-Thu
  )
  v <- check_rota(rota, inputs)
  expect_equal(nrow(dplyr::filter(v, hard_soft == "hard", rule_id != "COVER")), 0)
  checked <- v |>
    dplyr::filter(hard_soft == "soft") |>
    dplyr::summarise(penalty = sum(penalty), .by = rule_id) |>
    dplyr::arrange(rule_id)
  expect_equal(checked$penalty[checked$rule_id == "BLOCK_REST"], 3000)
  expect_equal(checked$penalty[checked$rule_id == "CONSEC_NIGHT_WEEKS"], 1000)
  expect_equal(checked$penalty[checked$rule_id == "PATTERN_SEQUENCE"], 500)

  modelled <- model_penalties(rota, inputs)
  expect_equal(modelled$rule, checked$rule_id)
  expect_equal(modelled$penalty, checked$penalty)
})

test_that("standing preferences cost each shift against them", {
  inputs <- consultant_inputs()
  v <- hits(rota_of(c("C04", "C02"), c("2027-02-02", "2027-02-05"), "2ND"), inputs)

  prefs <- v[v$rule_id == "PREFERENCES", ]
  expect_equal(prefs$doctor_id, c("C04", "C02"))
  expect_equal(prefs$penalty, c(5, 20))
  expect_true(all(grepl("prefers Tue off", prefs$detail[1]), grepl("prefers 24H", prefs$detail[2])))
})

test_that("the model scores a consultant rota exactly as the checker does", {
  inputs <- consultant_inputs(weeks = 3)
  rota <- dplyr::bind_rows(
    rota_of("C01", days_from("2027-02-02", 3), "1ST"),         # Tue-Thu: 3 days outside blocks
    rota_of("C03", days_from("2027-02-08", 4), "1ST"),         # Mon-Thu, but prefers 7 days
    rota_of("C04", "2027-02-02", "2ND"),                       # prefers Tuesdays off
    rota_of("C02", "2027-02-05", "2ND"),                       # prefers 24-hour shifts
    rota_of("C10", as.Date("2027-02-01") + c(0, 2, 4, 6), "NIGHT"), # avoids this block
    rota_of("C05", "2027-02-09", "24H"),                       # a night outside a block
    rota_of("C06", days_from("2027-02-15", 7), "1ST")          # 7 days without preferring them
  )
  checked <- check_rota(rota, inputs) |>
    dplyr::filter(hard_soft == "soft") |>
    dplyr::summarise(penalty = sum(penalty), .by = rule_id) |>
    dplyr::arrange(rule_id)
  expect_equal(nrow(dplyr::filter(check_rota(rota, inputs), hard_soft == "hard", rule_id != "COVER")), 0)
  expect_equal(checked$penalty[checked$rule_id == "PATTERNS"], 400)
  expect_equal(checked$penalty[checked$rule_id == "PATTERN_UNPREFERRED"], 100)

  modelled <- model_penalties(rota, inputs)
  expect_equal(modelled$rule, checked$rule_id)
  expect_equal(modelled$penalty, checked$penalty)
})

test_that("a solved consultant rota fills every slot, passes the hard rules and scores as checked", {
  inputs <- consultant_inputs(weeks = 2)
  solution <- solve_rota(inputs, time_limit = 30)

  expect_equal(solution$status, "Feasible")
  v <- check_rota(solution$rota, inputs)
  expect_equal(nrow(v[v$hard_soft == "hard", ]), 0)

  checked <- v |>
    dplyr::summarise(penalty = sum(penalty), .by = rule_id) |>
    dplyr::arrange(rule_id)
  solved <- solution$penalties |>
    dplyr::filter(!startsWith(rule, "FAIR_"), penalty != 0) |>
    dplyr::arrange(rule)
  expect_equal(solved$rule, checked$rule_id)
  expect_equal(solved$penalty, checked$penalty)

  doctors <- summarise_doctors(solution$rota, inputs)
  expect_true(all(c("1ST", "fair_1ST", "2ND", "NIGHT", "fair_NIGHT") %in% names(doctors)))
  # Each 24-hour shift counts towards both 2nd on and nights
  slots <- rota_slots(solution$rota, inputs)
  expect_equal(sum(doctors$NIGHT), sum(slots$slot == "NIGHT"))
  expect_equal(sum(doctors$NIGHT), 14)
})
