example_rota <- function() {
  read_rota(test_path("fixtures", "example_rota_output.xlsx"))
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
  expect_equal(first_day$ICU2_LD, "UNFILLED")
  expect_equal(by_shift$ICU_DAY[6], "UNFILLED") # 6 Feb is a Saturday: one short day
  expect_true(is.na(by_shift$ANA_DAY[6])) # not required at weekends
})

test_that("calendars are valid iCalendar with shifts and approved leave", {
  inputs <- read_rota_inputs(rota_example())
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

test_that("fair shares are split within each staffing pool", {
  inputs <- test_inputs()
  doctors <- summarise_doctors(example_rota(), inputs)

  expect_setequal(unique(doctors$pool), c("anaesthetics", "icu_tier2"))
  by_pool <- dplyr::summarise(doctors, dplyr::across(c(on_calls, fair_on_calls, weekends, fair_weekends), sum),
                              .by = pool)
  expect_equal(by_pool$fair_on_calls, by_pool$on_calls, tolerance = 0.5)
  expect_equal(by_pool$fair_weekends, by_pool$weekends, tolerance = 0.5)
})

test_that("the report shows each doctor's distance from their fair share", {
  inputs <- test_inputs()
  rota <- example_rota()
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_check_report(check_rota(rota, inputs), summarise_doctors(rota, inputs), path)

  doctors <- readxl::read_excel(path, "doctors")
  expect_equal(doctors$on_calls_vs_fair, round(doctors$on_calls - doctors$fair_on_calls, 1))
  expect_true(all(c("nights_vs_fair", "weekends_vs_fair") %in% names(doctors)))
  expect_equal(doctors$hours_vs_contracted, round(doctors$avg_weekly_hours - doctors$contracted_hours, 1))
})

test_that("patterns count runs, single nights, split weekends and single days off", {
  inputs <- two_weeks(test_inputs())
  # Same D01 rota as the model-vs-checker test, plus 3 nights for D06
  rota <- dplyr::bind_rows(
    rota_of("D01", days_from("2027-02-01", 5), "ICU_DAY"),
    rota_of("D01", c("2027-02-06", "2027-02-08", "2027-02-09", "2027-02-11"),
            c("ICU1_LD", "ICU_DAY", "ICU_DAY", "ICU1_N")),
    rota_of("D06", days_from("2027-02-01", 3), "ANA1_N")
  )
  p <- summarise_patterns(rota, inputs)
  d01 <- p[p$doctor_id == "D01", ]
  expect_equal(d01$shift_runs, 3)
  expect_equal(d01$longest_run, 6)
  expect_equal(d01$long_runs, 1)
  expect_equal(d01$single_nights, 1)
  expect_equal(d01$icu_runs, 2) # Mon 1 - Sat 6, Mon 8 - Tue 9
  expect_equal(d01$short_icu_runs, 1) # Mon 8 - Tue 9; the night is not an ICU day shift
  expect_equal(d01$split_weekends, 1)
  expect_equal(d01$single_days_off, 2)
  expect_equal(p$longest_night_run[p$doctor_id == "D06"], 3)

  all <- p[p$doctor_id == "All", ]
  expect_equal(all$night_runs, 2)
  expect_equal(all$longest_run, 6)
  expect_equal(nrow(p), nrow(inputs$staff) + 1)

  path <- withr::local_tempfile(fileext = ".xlsx")
  write_check_report(check_rota(rota, inputs), summarise_doctors(rota, inputs), path,
                     patterns = p)
  expect_true("patterns" %in% readxl::excel_sheets(path))
})
