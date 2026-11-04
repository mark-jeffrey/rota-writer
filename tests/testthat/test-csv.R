read_csv_bom <- function(path) {
  utils::read.csv(path, fileEncoding = "UTF-8-BOM", colClasses = "character", na.strings = "")
}

test_that("write_rota_csv has one row per shift and leave day, with clock times", {
  inputs <- read_rota_inputs(rota_example())
  rota <- rota_of("D02", c("2027-02-01", "2027-02-02"), c("ANA1_N", "ANA_DAY"))
  path <- withr::local_tempfile(fileext = ".csv")
  write_rota_csv(rota, inputs, path)

  # Excel needs the byte order mark to read UTF-8
  expect_equal(readBin(path, "raw", 3), as.raw(c(0xef, 0xbb, 0xbf)))
  x <- read_csv_bom(path)
  expect_equal(names(x), c("doctor_id", "name", "grade", "date", "day", "code", "kind", "description",
                           "area", "shift_type", "start", "end", "hours"))

  shifts <- x[x$kind == "shift", ]
  expect_equal(shifts$code, c("ANA1_N", "ANA_DAY"))
  expect_equal(shifts$day, c("Mon", "Tue"))
  expect_equal(shifts$description[1], "ANA1 night")
  # A night ends the next morning
  expect_equal(shifts$start[1], "2027-02-01 20:00")
  expect_equal(shifts$end[1], "2027-02-02 08:30")
  expect_equal(as.numeric(shifts$hours), c(12.5, 9))

  # D02's approved annual leave, 17-21 Feb: one row a day, with no times
  leave <- x[x$kind == "leave" & x$doctor_id == "D02" & x$date < "2027-03-01", ]
  expect_equal(leave$date, format(as.Date("2027-02-17") + 0:4))
  expect_equal(unique(leave$code), "AL")
  expect_equal(unique(leave$description), "Annual leave")
  expect_true(all(is.na(leave$start) & is.na(leave$hours)))
})

test_that("write_rota_csv has the same shifts as the rota workbook", {
  inputs <- test_inputs()
  rota <- read_rota(test_path("fixtures", "example_rota_output.xlsx"))
  path <- withr::local_tempfile(fileext = ".csv")
  write_rota_csv(rota, inputs, path)

  x <- read_csv_bom(path)
  shifts <- x[x$kind == "shift", c("doctor_id", "date", "code")] |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::arrange(date, doctor_id, code)
  expected <- rota |>
    dplyr::semi_join(inputs$shifts, by = c(code = "shift_code")) |>
    dplyr::filter(date >= inputs$period$start, date <= inputs$period$end) |>
    dplyr::select(doctor_id, date, code) |>
    dplyr::arrange(date, doctor_id, code)
  expect_equal(tibble::as_tibble(shifts), expected, ignore_attr = TRUE)
})

test_that("write_violations_csv has one row per rule broken", {
  inputs <- test_inputs()
  rota <- read_rota(test_path("fixtures", "example_rota_output.xlsx"))
  violations <- check_rota(rota, inputs)
  path <- withr::local_tempfile(fileext = ".csv")
  write_violations_csv(violations, path)

  x <- read_csv_bom(path)
  expect_equal(nrow(x), nrow(violations))
  expect_equal(names(x), names(violations))
  expect_equal(x$start_date, format(violations$start_date))
})
