example_rota <- function() {
  read_rota(test_path("fixtures", "example_rota_output.xlsx"))
}

test_that("write_rota round-trips through read_rota and has every sheet", {
  inputs <- test_inputs()
  rota <- example_rota()
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_rota(rota, inputs, path)

  expect_equal(readxl::excel_sheets(path), c("by_role", "by_doctor", "doctors", "key", "long"))
  back <- read_rota(path) |>
    dplyr::semi_join(inputs$shifts, by = c(code = "shift_code"))
  shifts <- rota |>
    dplyr::semi_join(inputs$shifts, by = c(code = "shift_code"))
  expect_equal(dplyr::arrange(back, date, doctor_id), dplyr::arrange(shifts, date, doctor_id))

  grid <- readxl::read_excel(path, "by_doctor", skip = 1, n_max = 91)
  expect_equal(nrow(grid), 91)
  expect_true("D01 Ada Alder" %in% names(grid))
  # Most senior grade first, under a band of grades
  ranks <- inputs$grades$rank[match(inputs$staff$grade, inputs$grades$grade)]
  doctors <- setdiff(names(grid), c("date", "day", "holiday"))
  expect_equal(doctors, unname(doctor_labels(inputs$staff)[order(-ranks)]))
  band <- readxl::read_excel(path, "by_doctor", range = "D1:D1", col_names = FALSE)[[1]]
  expect_equal(band, inputs$staff$grade[order(-ranks)][1])
  expect_equal(sum(!is.na(grid$holiday)), nrow(inputs$bank_holidays))
})

test_that("by_role shows who is on and flags unfilled slots", {
  inputs <- test_inputs()
  rota <- rota_of(c("D01", "D04"), "2027-02-01", c("ICU1_LD", "ICU_DAY"))
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_rota(rota, inputs, path)

  by_shift <- readxl::read_excel(path, "by_role", skip = 1)
  first_day <- by_shift[1, ]
  expect_equal(first_day$ICU1_LD, "Ada Alder")
  expect_equal(first_day$ICU_DAY, "Dev Damson, UNFILLED") # needs 2
  expect_equal(first_day$ICU1_N, "UNFILLED")
  expect_equal(first_day$ICU2_LD, "UNFILLED")
  expect_equal(by_shift$ICU_DAY[6], "UNFILLED") # 6 Feb is a Saturday: one short day
  expect_true(is.na(by_shift$ANA_DAY[6])) # not required at weekends

  # On-calls first, grouped by area, then normal days, under a band of areas
  expect_equal(names(by_shift)[-(1:3)],
               c("ICU1_LD", "ICU1_N", "ICU2_LD", "ICU2_N", "ANA1_LD", "ANA1_N", "ANA2_LD", "ANA2_N",
                 "OBS_LD", "OBS_N", "ICU_DAY", "ANA_DAY"))
  band <- unlist(readxl::read_excel(path, "by_role", range = "D1:O1", col_names = FALSE))
  expect_equal(unname(band[c(1, 5, 9, 11, 12)]),
               c("ICU", "Theatres", "Obstetrics", "ICU normal days", "Theatres normal days"))
})

test_that("each area has its own hue, toned by shift type", {
  colours <- code_colours(test_inputs()$shifts)
  fill <- set_names(colours$fill, colours$code)
  # Long day and night of a role share an area; areas differ
  expect_equal(colours$area[colours$code %in% c("ICU1_LD", "ICU2_N", "ICU_DAY")], rep("ICU", 3))
  expect_false(fill[["ICU1_LD"]] == fill[["ANA1_LD"]])
  expect_equal(fill[["ICU1_LD"]], fill[["ICU2_LD"]])
  # Nights are darker, with white text
  expect_lt(sum(col2rgb(fill[["ICU1_N"]])), sum(col2rgb(fill[["ICU1_LD"]])))
  expect_equal(colours$text[colours$code == "OBS_N"], "#FFFFFF")
  expect_equal(fill[["AL"]], rota_colours$fill[rota_colours$style == "leave"])

  # Without an area column, each role is its own area
  shifts <- dplyr::select(test_inputs()$shifts, -area)
  expect_equal(code_colours(shifts)$area[1:3], c("ICU_DAY", "ANA_DAY", "ICU1"))
})

test_that("calendars are valid iCalendar with shifts and approved leave", {
  inputs <- read_rota_inputs(rota_example())
  rota <- rota_of("D02", c("2027-02-01", "2027-02-02"), c("ANA1_N", "ANA_DAY"))
  dir <- withr::local_tempdir()
  paths <- write_calendars(rota, inputs, dir, doctors = "D02")

  expect_equal(basename(paths), c("D02_Ben_Birch.ics", "All_doctors.ics"))
  raw <- readBin(paths[1], "raw", file.size(paths[1]))
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

test_that("the all-doctors calendar has everyone's shifts as role: name, and no leave", {
  inputs <- read_rota_inputs(rota_example())
  rota <- rota_of(c("D01", "D02"), "2027-02-01", c("ICU1_N", "ANA_DAY"))
  dir <- withr::local_tempdir()
  write_calendars(rota, inputs, dir)

  lines <- strsplit(rawToChar(readBin(file.path(dir, "All_doctors.ics"), "raw", 1e5)), "\r\n")[[1]]
  expect_equal(sum(lines == "BEGIN:VEVENT"), 2)
  expect_true(all(c("SUMMARY:ICU1 night: Dr Ada Alder", "SUMMARY:ANA day: Dr Ben Birch") %in% lines))
  expect_false(any(grepl("leave", lines)))
  expect_false(any(duplicated(lines[startsWith(lines, "UID:")])))
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

test_that("doctors sharing a name keep their ID in lists of who is on", {
  staff <- tibble::tibble(doctor_id = c("D01", "D02", "D03"),
                          name = c("Dr Ann Ash", "Dr Ann Ash", "Dr Bo Bay"))
  expect_equal(unname(doctor_names(staff)), c("D01 Ann Ash", "D02 Ann Ash", "Bo Bay"))
})
