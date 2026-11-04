# Plain CSV versions of the rota and the checker's results, for other
# software: medical staffing systems, payroll checks, Power BI or a script.

#' Write a rota as CSV
#'
#' Saves a rota as a plain CSV file with one row per shift worked and one per
#' day of approved leave, for software other than Excel: medical staffing
#' and e-rostering systems, payroll checks, Power BI or a script. Unlike the
#' rota workbook, each row says when the shift starts and ends, so nights
#' that cross midnight need no working out.
#'
#' Columns:
#' * `doctor_id`, `name`, `grade`
#' * `date`: the day the shift starts (`YYYY-MM-DD`), and `day` (`Mon`...)
#' * `code`: the shift code, or `AL`, `SL`, `EX` or `PL` for leave
#' * `kind`: `shift` or `leave`
#' * `description`: e.g. `ICU1 night` or `Annual leave`
#' * `area` and `shift_type` (`normal_day`, `long_day`, `night`...)
#' * `start`, `end`: local clock times, `YYYY-MM-DD HH:MM`; blank for leave
#' * `hours`: the shift's hours; blank for leave
#'
#' The file is UTF-8 with a byte order mark, so Excel shows accented names
#' correctly, and rows are in date then doctor order.
#'
#' @inheritParams write_rota
#' @param path Path of the `.csv` file to write. An existing file is
#'   overwritten.
#'
#' @returns `path`, invisibly.
#' @seealso [write_rota()] for the coordinator's workbook,
#'   [write_violations_csv()] for the checker's results.
#' @export
#' @examples
#' inputs <- read_rota_inputs(rota_example())
#' rota <- read_rota(rota_example("residents", "output/rota.xlsx"))
#' write_rota_csv(rota, inputs, tempfile(fileext = ".csv"))
write_rota_csv <- function(rota, inputs, path) {
  inputs <- as_rota_inputs(inputs)
  rota <- as_rota(rota)
  clock <- function(x) format(x, "%Y-%m-%d %H:%M", tz = "UTC")

  shifts <- shift_instances(rota, inputs) |>
    filter(date >= inputs$period$start, date <= inputs$period$end) |>
    transmute(
      doctor_id, date, code, kind = "shift",
      description = shift_label(role, shift_type, label),
      area = as.character(shift_areas(inputs$shifts))[match(code, inputs$shifts$shift_code)],
      shift_type, start = clock(start_dt), end = clock(end_dt), hours
    )
  leave <- rota_with_leave(rota, inputs) |>
    filter(code %in% leave_codes) |>
    transmute(
      doctor_id, date, code, kind = "leave",
      description = paste(stringr::str_to_title(names(leave_codes)[match(code, leave_codes)]), "leave")
    )

  staff <- inputs$staff
  out <- bind_rows(shifts, leave) |>
    mutate(
      name = staff$name[match(doctor_id, staff$doctor_id)],
      grade = (staff[["grade"]] %||% rep(NA_character_, nrow(staff)))[match(doctor_id, staff$doctor_id)],
      day = format(date, "%a"),
      date = format(date, "%Y-%m-%d")
    ) |>
    arrange(date, match(doctor_id, staff$doctor_id), start) |>
    select(doctor_id, name, grade, date, day, code, kind, description, area, shift_type,
           start, end, hours)
  write_csv_utf8(out, path)
}

#' Write the checker's results as CSV
#'
#' Saves the output of [check_rota()] as a plain CSV file with one row per
#' rule broken, the same as the `violations` sheet of the compliance report.
#' Use it to count or track breaches across rotas in other software.
#'
#' @inheritParams write_check_report
#' @param path Path of the `.csv` file to write. An existing file is
#'   overwritten.
#'
#' @returns `path`, invisibly.
#' @seealso [write_check_report()] for the coordinator's workbook.
#' @export
#' @examples
#' inputs <- read_rota_inputs(rota_example())
#' rota <- read_rota(rota_example("residents", "output/rota.xlsx"))
#' write_violations_csv(check_rota(rota, inputs), tempfile(fileext = ".csv"))
write_violations_csv <- function(violations, path) {
  violations |>
    mutate(across(where(lubridate::is.Date), \(x) format(x, "%Y-%m-%d"))) |>
    write_csv_utf8(path)
}

# CSV with a byte order mark, so Excel reads it as UTF-8; blank for missing
write_csv_utf8 <- function(x, path) {
  con <- file(path, open = "wb", encoding = "UTF-8")
  on.exit(close(con))
  writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con)
  utils::write.csv(x, con, row.names = FALSE, na = "")
  invisible(path)
}
