test_that("test workbook reads and validates cleanly", {
  inputs <- read_rota_inputs(system.file("extdata", "rota_inputs.xlsx", package = "rotawriter"))

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
