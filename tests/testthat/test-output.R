example_rota <- function() {
  read_rota(system.file("extdata", "example_rota_output.xlsx", package = "rotawriter"))
}

test_that("write_rota round-trips through read_rota and has every sheet", {
  inputs <- test_inputs()
  rota <- example_rota()
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_rota(rota, inputs, path)

  expect_equal(readxl::excel_sheets(path), c("grid", "by_shift", "doctors", "key", "long"))
  back <- read_rota(path) |>
    dplyr::semi_join(inputs$shifts, by = c(code = "shift_code"))
  shifts <- rota |>
    dplyr::semi_join(inputs$shifts, by = c(code = "shift_code"))
  expect_equal(dplyr::arrange(back, date, doctor_id), dplyr::arrange(shifts, date, doctor_id))

  grid <- readxl::read_excel(path, "grid")
  expect_equal(nrow(grid), 91)
  expect_true("D01 Ada Alder" %in% names(grid))
  expect_equal(sum(!is.na(grid$holiday)), nrow(inputs$bank_holidays))
})

test_that("by_shift shows who is on and flags unfilled slots", {
  inputs <- test_inputs()
  rota <- rota_of(c("D01", "D04"), "2027-02-01", c("ICU1_LD", "ICU_DAY"))
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_rota(rota, inputs, path)

  by_shift <- readxl::read_excel(path, "by_shift")
  first_day <- by_shift[1, ]
  expect_equal(first_day$ICU1_LD, "D01 Ada Alder")
  expect_equal(first_day$ICU_DAY, "D04 Dev Damson, UNFILLED") # needs 2
  expect_equal(first_day$ICU1_N, "UNFILLED")
  expect_true(is.na(first_day$ICU2_LD)) # not required on weekdays
})

test_that("calendars are valid iCalendar with shifts and approved leave", {
  inputs <- read_rota_inputs(system.file("extdata", "rota_inputs.xlsx", package = "rotawriter"))
  rota <- rota_of("D02", c("2027-02-01", "2027-02-02"), c("ANA1_N", "ANA_DAY"))
  dir <- withr::local_tempdir()
  paths <- write_calendars(rota, inputs, dir, doctors = "D02")

  expect_equal(basename(paths), "D02_Ben_Birch.ics")
  raw <- readBin(paths, "raw", file.size(paths))
  expect_true(grepl("\r\n", rawToChar(raw), fixed = TRUE))
  lines <- strsplit(rawToChar(raw), "\r\n")[[1]]

  expect_equal(lines[1], "BEGIN:VCALENDAR")
  expect_equal(lines[length(lines)], "END:VCALENDAR")
  expect_true(all(nchar(lines, type = "bytes") <= 75))
  expect_equal(sum(lines == "BEGIN:VEVENT"), sum(lines == "END:VEVENT"))
  # The night ends the next morning
  expect_true(all(c("DTSTART:20270201T200000", "DTEND:20270202T083000") %in% lines))
  expect_true("SUMMARY:ANA day" %in% lines)
  # D02's approved annual leave, 17-21 Feb, as an all-day event ending the day after
  expect_true(all(c("DTSTART;VALUE=DATE:20270217", "DTEND;VALUE=DATE:20270222") %in% lines))
})

test_that("iCalendar text is escaped and long lines folded", {
  expect_equal(ics_text("a, b; c\\d\ne"), "a\\, b\\; c\\\\d\\ne")
  folded <- ics_fold(paste0("DESCRIPTION:", strrep("é", 100)))
  pieces <- strsplit(folded, "\r\n")[[1]]
  expect_true(all(nchar(pieces, type = "bytes") <= 75))
  expect_equal(paste0(pieces[1], paste(substring(pieces[-1], 2), collapse = "")),
               paste0("DESCRIPTION:", strrep("é", 100)))
})

test_that("nights-exempt doctors have no fair share of nights", {
  inputs <- test_inputs()
  doctors <- summarise_doctors(example_rota(), inputs)

  exempt <- inputs$staff$doctor_id[inputs$staff$nights_exempt]
  expect_true(all(doctors$fair_nights[doctors$doctor_id %in% exempt] == 0))
  expect_equal(sum(doctors$fair_nights), sum(doctors$nights), tolerance = 0.5)
})

test_that("the report shows each doctor's distance from their fair share", {
  inputs <- test_inputs()
  rota <- example_rota()
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_check_report(check_rota(rota, inputs), summarise_doctors(rota, inputs), path)

  doctors <- readxl::read_excel(path, "doctors")
  expect_equal(doctors$on_calls_vs_fair, round(doctors$on_calls - doctors$fair_on_calls, 1))
  expect_true(all(c("nights_vs_fair", "weekends_vs_fair") %in% names(doctors)))
})
