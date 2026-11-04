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
  expect_equal(unclass(read_rota_inputs(path))[names(inputs)], unclass(inputs))
})
