test_that("test workbook reads and validates cleanly", {
  inputs <- read_rota_inputs(rota_example())

  expect_equal(inputs$period$start, as.Date("2027-02-01"))
  expect_equal(inputs$period$end, as.Date("2027-05-02"))
  expect_type(inputs$staff$can_OBS, "logical")
  expect_s3_class(inputs$leave$start_date, "Date")
  expect_equal(nrow(validate_inputs(inputs)), 0)
})

test_that("validate_inputs reports broken references and shift hours", {
  inputs <- test_inputs()
  inputs$leave <- tibble::tibble(doctor_id = "D99", start_date = as.Date("2027-02-01"),
                                 end_date = as.Date("2027-01-31"), leave_type = "annual",
                                 status = "approved")
  inputs$shifts$hours[inputs$shifts$shift_code == "ICU1_N"] <- 10

  problems <- validate_inputs(inputs)$problem
  expect_true(any(grepl("Unknown doctor_id: D99", problems)))
  expect_true(any(grepl("Leave ends before it starts", problems)))
  expect_true(any(grepl("ICU1_N: hours", problems)))
})

test_that("validate_inputs reports in-post days with no placement", {
  inputs <- test_inputs()
  inputs$placements <- inputs$placements[inputs$placements$doctor_id != "D01", ]

  expect_true(any(grepl("D01 has no placement", validate_inputs(inputs)$problem)))
})

test_that("the rota type sets which rules are on when the rules sheet leaves them out", {
  inputs <- test_inputs()
  inputs$rules <- inputs$rules[inputs$rules$rule_id != "MIN_REST", ]
  expect_true(rule_on(inputs, "MIN_REST"))

  inputs$settings <- tibble::add_row(inputs$settings, key = "rota_type", value = "consultant")
  expect_false(rule_on(inputs, "MIN_REST"))
  expect_true(rule_on(inputs, "COVER"))

  inputs$settings$value[inputs$settings$key == "rota_type"] <- "surgeons"
  expect_error(rule_on(inputs, "MIN_REST"), "Unknown rota_type \"surgeons\"")
})

test_that("sheets a rota type does not need are read as empty", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  sheets <- map(set_names(setdiff(readxl::excel_sheets(rota_example()), "about")),
                \(s) readxl::read_excel(rota_example(), sheet = s))
  sheets$settings <- tibble::add_row(sheets$settings, key = "rota_type", value = "consultant")
  sheets[c("grades", "placements", "placement_roles", "requests", "skill_mix")] <- NULL
  openxlsx2::write_xlsx(sheets, path)

  inputs <- read_rota_inputs(path)
  expect_equal(nrow(inputs$placements), 0)
  expect_equal(nrow(inputs$patterns), 0)
  expect_s3_class(inputs$requests$date, "Date")

  sheets$settings$value[sheets$settings$key == "rota_type"] <- "resident"
  openxlsx2::write_xlsx(sheets, path)
  expect_error(read_rota_inputs(path), "Missing sheets.*grades")
})

test_that("the about sheet is skipped and sheets may be in any order", {
  expect_equal(readxl::excel_sheets(rota_example())[1], "about")
  inputs <- read_rota_inputs(rota_example())
  expect_false("about" %in% names(inputs))

  path <- withr::local_tempfile(fileext = ".xlsx")
  sheets <- map(set_names(rev(setdiff(readxl::excel_sheets(rota_example()), "about"))),
                \(s) readxl::read_excel(rota_example(), sheet = s))
  openxlsx2::write_xlsx(sheets, path)
  expect_equal(unclass(read_rota_inputs(path))[names(inputs)], unclass(inputs)[names(inputs)])
})

# The example workbook with some cells changed, written out and read back
edited_inputs <- function(...) {
  path <- withr::local_tempfile(fileext = ".xlsx", .local_envir = parent.frame())
  sheets <- map(set_names(setdiff(readxl::excel_sheets(rota_example()), "about")),
                \(s) readxl::read_excel(rota_example(), sheet = s))
  for (edit in list(...)) sheets <- edit(sheets)
  openxlsx2::write_xlsx(sheets, path)
  read_rota_inputs(path)
}

set_rule <- function(id, col, value) {
  function(sheets) {
    sheets$rules[[col]] <- as.character(sheets$rules[[col]])
    sheets$rules[[col]][sheets$rules$rule_id == id] <- value
    sheets
  }
}

test_that("codes are read whatever their case or spacing", {
  inputs <- edited_inputs(\(s) {
    s$leave$status[1] <- " Approved"
    s$leave$leave_type[1] <- "ANNUAL"
    s$requests$request[1] <- "No_Night "
    s
  })
  expect_equal(inputs$leave$status[1], "approved")
  expect_equal(inputs$leave$leave_type[1], "annual")
  expect_equal(inputs$requests$request[1], "no_night")
  expect_equal(nrow(validate_inputs(inputs)), 0)
})

test_that("times typed as 8:00 or 08:00:00 are read as HH:MM", {
  expect_equal(as_hhmm(c("8:00", "08:00:00", " 20:30", "0.375", "25:00", "8am", NA)),
               c("08:00", "08:00", "20:30", "09:00", "25:00", "8am", NA))
  inputs <- edited_inputs(\(s) {
    s$shifts$start <- as.character(s$shifts$start)
    s$shifts$start[1] <- "8am"
    s
  })
  problems <- validate_inputs(inputs)$problem
  expect_true(any(grepl("start and end should be times like 08:00", problems)))
  expect_error(build_rota_model(inputs), "valid start time")
})

test_that("validate_inputs reports unknown leave statuses, leave types and requests", {
  inputs <- test_inputs()
  inputs$leave <- tibble::tibble(doctor_id = "D01", start_date = as.Date("2027-02-01"),
                                 end_date = as.Date("2027-02-02"), leave_type = "holiday",
                                 status = "agreed")
  inputs$requests <- tibble::tibble(doctor_id = "D01", date = as.Date("2027-02-03"),
                                    request = "no-night", weight = 1)

  problems <- validate_inputs(inputs)$problem
  expect_true(any(grepl("Leave status for D01 .* \"agreed\"", problems)))
  expect_true(any(grepl("Leave type for D01 .* \"holiday\"", problems)))
  expect_true(any(grepl("Unknown request for D01 .*\"no-night\"", problems)))
})

test_that("validate_inputs reports a blank or missing WTE", {
  inputs <- test_inputs()
  inputs$staff$wte[1] <- NA
  expect_true(any(grepl(paste("WTE should be between 0.1 and 1 for", inputs$staff$doctor_id[1]),
                        validate_inputs(inputs)$problem)))
})

test_that("text in a number column is reported, and the rest of the column still reads", {
  inputs <- edited_inputs(set_rule("MAX_7DAY", "value", "72 h"))
  expect_type(inputs$rules$value, "double")
  expect_equal(rule_value(inputs, "MIN_REST"), 11)
  problems <- validate_inputs(inputs)$problem
  expect_true(any(grepl("MAX_7DAY value: \"72 h\" is not a number", problems)))
  expect_true(any(grepl("MAX_7DAY is on but its value is blank", problems)))
})

test_that("a Y/N cell that is neither is reported", {
  inputs <- edited_inputs(set_rule("MIN_REST", "enabled", "maybe"), set_rule("MAX_7DAY", "enabled", "yes"))
  expect_true(rule_on(inputs, "MAX_7DAY"))
  expect_true(any(grepl("MIN_REST enabled: \"maybe\" should be Y or N", validate_inputs(inputs)$problem)))
})

test_that("a rule that is on needs one value", {
  inputs <- test_inputs()

  blank <- inputs
  blank$rules$value[blank$rules$rule_id == "MIN_REST"] <- NA
  expect_true(any(grepl("MIN_REST is on but its value is blank", validate_inputs(blank)$problem)))
  expect_error(check_rota(rota_example("residents", "output/rota.xlsx"), blank),
               "MIN_REST is on but has no value")
  expect_error(build_rota_model(blank), "MIN_REST is on but has no value")

  # Switched off, it needs no value
  blank$rules$enabled[blank$rules$rule_id == "MIN_REST"] <- FALSE
  expect_false(any(grepl("MIN_REST", validate_inputs(blank)$problem)))

  # Resident rules left out of the sheet are on, so they need a row
  missing <- inputs
  missing$rules <- missing$rules[missing$rules$rule_id != "MAX_7DAY", ]
  expect_true(any(grepl("MAX_7DAY is on but has no row", validate_inputs(missing)$problem)))

  twice <- inputs
  twice$rules <- dplyr::bind_rows(twice$rules, twice$rules[twice$rules$rule_id == "MIN_REST", ])
  expect_true(any(grepl("MIN_REST is listed more than once", validate_inputs(twice)$problem)))
  expect_error(rule_value(twice, "MIN_REST"), "more than once")

  typo <- inputs
  typo$rules$rule_id[typo$rules$rule_id == "SINGLE_NIGHT"] <- "SINGLE_NIGHTS"
  expect_true(any(grepl("Unknown rule_id: SINGLE_NIGHTS", validate_inputs(typo)$problem)))
})

test_that("approved leave typed with a capital is still honoured by the checker", {
  rota <- read_rota(rota_example("residents", "output/rota.xlsx"))
  shift <- rota[!rota$code %in% c("AL", "SL", "EX", "PL"), ][1, ]
  inputs <- edited_inputs(\(s) {
    s$leave <- tibble::tibble(doctor_id = shift$doctor_id, start_date = shift$date,
                              end_date = shift$date, leave_type = "annual", status = "Approved")
    s
  })
  expect_true("LEAVE_APPROVED" %in% check_rota(rota, inputs)$rule_id)
})
