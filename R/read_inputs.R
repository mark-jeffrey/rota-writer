# Reading and validating the rota input workbook and rotas.

required_sheets <- c(
  "settings", "rules", "grades", "shifts", "cover", "bank_holidays", "staff",
  "placements", "placement_roles", "leave", "requests", "skill_mix"
)

yn_columns <- c("is_on_call", "is_night", "is_long", "is_late",
                "nights_exempt", "opted_out_wtr", "enabled")

#' Read the rota input workbook
#'
#' Reads every sheet of the inputs workbook (staff, shifts, cover, rules,
#' leave, requests, ...) into a list of tibbles, ready for [check_rota()]
#' and, later, the solver. Dates become `Date`, `Y`/`N` columns become
#' logical, and times become `"HH:MM"` text even if Excel has turned them
#' into time values.
#'
#' Run [validate_inputs()] afterwards to catch inconsistent data, such as
#' leave for a doctor who is not in the staff list.
#'
#' @param path Path to an `.xlsx` workbook with the sheets `settings`,
#'   `rules`, `grades`, `shifts`, `cover`, `bank_holidays`, `staff`,
#'   `placements`, `placement_roles`, `leave`, `requests` and `skill_mix`.
#'   See `inst/extdata/rota_inputs.xlsx` for the layout.
#'
#' @returns A `rota_inputs` object: a named list with one tibble per sheet
#'   (e.g. `inputs$staff`), plus `period`, a list holding the rota's `start`
#'   and `end` dates.
#'
#' @export
#' @examples
#' inputs <- read_rota_inputs(system.file("extdata", "rota_inputs.xlsx", package = "rotawriter"))
#' inputs
#' inputs$staff
read_rota_inputs <- function(path) {
  check_file(path, "inputs workbook")
  sheets <- readxl::excel_sheets(path)
  missing <- setdiff(required_sheets, sheets)
  if (length(missing) > 0) {
    stop("Missing sheets in ", path, ": ", paste(missing, collapse = ", "), call. = FALSE)
  }

  inputs <- set_names(sheets) |>
    map(\(s) readxl::read_excel(path, sheet = s)) |>
    map(tidy_sheet)

  setting <- set_names(inputs$settings$value, inputs$settings$key)
  inputs$period <- list(
    start = parse_date(setting[["rota_start"]]),
    end   = parse_date(setting[["rota_end"]])
  )
  structure(inputs, class = c("rota_inputs", "list"))
}

#' @export
print.rota_inputs <- function(x, ...) {
  sheets <- setdiff(names(x), "period")
  rows <- map_int(unclass(x)[sheets], nrow)
  cat("<rota_inputs>", format(x$period$start), "to", format(x$period$end), "\n")
  cat(sprintf("  %-16s %4d rows\n", sheets, rows), sep = "")
  invisible(x)
}

# Accept a rota_inputs object or a path to one
as_rota_inputs <- function(inputs) {
  if (is.character(inputs) && length(inputs) == 1) {
    return(read_rota_inputs(inputs))
  }
  if (is.data.frame(inputs)) {
    stop("`inputs` looks like a rota. Arguments are (rota, inputs): check_rota(rota, inputs)",
         call. = FALSE)
  }
  if (!inherits(inputs, "rota_inputs")) {
    stop("`inputs` must come from read_rota_inputs(), or be a path to an inputs workbook",
         call. = FALSE)
  }
  inputs
}

# Accept a long-format rota data frame or a path to a rota workbook
as_rota <- function(rota) {
  if (is.character(rota) && length(rota) == 1) {
    return(read_rota(rota))
  }
  if (inherits(rota, "rota_inputs")) {
    stop("`rota` looks like the inputs. Arguments are (rota, inputs): check_rota(rota, inputs)",
         call. = FALSE)
  }
  if (!is.data.frame(rota)) {
    stop("`rota` must be a data frame from read_rota(), or a path to a rota workbook",
         call. = FALSE)
  }
  check_rota_columns(rota)
  rota
}

check_rota_columns <- function(rota, where = "`rota`") {
  missing <- setdiff(c("doctor_id", "date", "code"), names(rota))
  if (length(missing) > 0) {
    stop(where, " must be in long format with columns doctor_id, date and code ",
         "(missing: ", paste(missing, collapse = ", "), "). ",
         "In a rota workbook this is the \"long\" sheet, not the \"grid\".", call. = FALSE)
  }
}

check_file <- function(path, what) {
  if (!is.character(path) || length(path) != 1) {
    stop("Expected a path to the ", what, call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Can't find the ", what, " at ", path, call. = FALSE)
  }
}

tidy_sheet <- function(df) {
  df |>
    mutate(
      across(any_of(c("date", "start_date", "end_date")), as.Date),
      across(any_of(c("start", "end")), as_hhmm),
      across(any_of(yn_columns) | starts_with("can_"), \(x) toupper(x) == "Y")
    )
}

# Settings are text, but a user may type a real date into Excel
parse_date <- function(x) {
  if (grepl("^[0-9]+(\\.0+)?$", x)) {
    return(as.Date(as.numeric(x), origin = "1899-12-30"))
  }
  as.Date(x)
}

# Times are "HH:MM" text, but Excel may have turned them into times
as_hhmm <- function(x) {
  if (inherits(x, "POSIXt")) format(x, "%H:%M", tz = "UTC") else as.character(x)
}

#' Read a rota
#'
#' Reads a rota in long format: one row per doctor per working day. This is
#' the format [check_rota()] uses. A rota workbook also has a `grid` sheet
#' (dates down, doctors across) for people to read, but that is not read
#' here.
#'
#' @param path Path to a rota workbook, such as
#'   `inst/extdata/example_rota_output.xlsx`.
#' @param sheet Name of the sheet holding the long-format rota, with
#'   columns `doctor_id`, `date` and `code`. Other columns are dropped.
#'
#' @returns A tibble with columns `doctor_id` (text), `date` (`Date`) and
#'   `code` (a shift code such as `ICU1_N`, or a leave code such as `AL`).
#'   Blank rows are dropped.
#'
#' @export
#' @examples
#' read_rota(system.file("extdata", "example_rota_output.xlsx", package = "rotawriter"))
read_rota <- function(path, sheet = "long") {
  check_file(path, "rota workbook")
  rota <- readxl::read_excel(path, sheet = sheet)
  check_rota_columns(rota, where = paste0("Sheet \"", sheet, "\""))

  rota |>
    transmute(doctor_id, date = as.Date(date), code) |>
    filter(!is.na(doctor_id), !is.na(code))
}

#' Validate the rota inputs
#'
#' Checks that the inputs workbook is consistent before any rota is checked
#' or solved. It reports:
#' * doctor IDs in `placements`, `leave` or `requests` that are not in `staff`
#' * duplicate doctor IDs, unknown grades, and WTE outside 0.1-1
#' * leave that ends before it starts
#' * shifts whose `hours` do not match their start and end times
#' * unknown shift codes, roles or grades in `cover`, `placement_roles` and
#'   `skill_mix`
#' * in-post days with no placement
#'
#' @inheritParams check_rota
#'
#' @returns A tibble with one row per problem: `sheet` (where to fix it) and
#'   `problem` (description). Zero rows means the inputs are consistent.
#'
#' @export
#' @examples
#' inputs <- read_rota_inputs(system.file("extdata", "rota_inputs.xlsx", package = "rotawriter"))
#' validate_inputs(inputs)
validate_inputs <- function(inputs) {
  inputs <- as_rota_inputs(inputs)
  ids <- inputs$staff$doctor_id
  problem <- function(sheet, failed, msg) {
    tibble::tibble(sheet = sheet, problem = msg[failed])
  }
  unknown_ids <- function(sheet) {
    bad <- setdiff(inputs[[sheet]]$doctor_id, ids)
    problem(sheet, seq_along(bad), paste("Unknown doctor_id:", bad))
  }

  shifts <- inputs$shifts
  clock_hours <- shift_clock_hours(shifts$start, shifts$end)
  skill_shifts <- unique(unlist(stringr::str_split(inputs$skill_mix$shifts, ";")))

  list(
    problem("staff", anyDuplicated(ids) > 0, "Duplicate doctor_id in staff"),
    problem("staff", which(!inputs$staff$grade %in% inputs$grades$grade),
            paste("Unknown grade:", inputs$staff$grade)),
    problem("staff", which(!between(inputs$staff$wte, 0.1, 1)),
            paste("WTE out of range for", ids)),
    unknown_ids("placements"),
    unknown_ids("leave"),
    unknown_ids("requests"),
    problem("leave", which(inputs$leave$end_date < inputs$leave$start_date),
            paste("Leave ends before it starts for", inputs$leave$doctor_id)),
    problem("shifts", which(abs(clock_hours - shifts$hours) > 0.01),
            paste0(shifts$shift_code, ": hours (", shifts$hours, ") do not match ",
                   shifts$start, "-", shifts$end)),
    problem("cover", which(!inputs$cover$shift_code %in% shifts$shift_code),
            paste("Unknown shift_code:", inputs$cover$shift_code)),
    problem("placement_roles", which(!inputs$placement_roles$role %in% shifts$role),
            paste("Unknown role:", inputs$placement_roles$role)),
    problem("skill_mix", which(!skill_shifts %in% shifts$shift_code),
            paste("Unknown shift_code:", skill_shifts)),
    problem("skill_mix", which(!inputs$skill_mix$min_grade %in% inputs$grades$grade),
            paste("Unknown grade:", inputs$skill_mix$min_grade)),
    missing_placements(inputs)
  ) |>
    list_rbind()
}

shift_clock_hours <- function(start, end) {
  to_h <- \(x) as.numeric(substr(x, 1, 2)) + as.numeric(substr(x, 4, 5)) / 60
  (to_h(end) - to_h(start)) %% 24
}

# Every in-post day should be covered by a placement
missing_placements <- function(inputs) {
  uncovered <- in_post_days(inputs) |>
    anti_join(
      inputs$placements,
      join_by(doctor_id, between(date, start_date, end_date))
    )
  if (nrow(uncovered) == 0) {
    return(NULL)
  }

  uncovered |>
    summarise(from = min(date), to = max(date), .by = doctor_id) |>
    transmute(sheet = "placements",
              problem = paste0(doctor_id, " has no placement for some days between ",
                               from, " and ", to))
}

in_post_days <- function(inputs) {
  inputs$staff |>
    select(doctor_id, start_date, end_date) |>
    tidyr::crossing(date = rota_dates(inputs)) |>
    filter(date >= start_date, date <= end_date) |>
    select(doctor_id, date)
}

rota_dates <- function(inputs) {
  seq(inputs$period$start, inputs$period$end, by = "day")
}

day_type <- function(date, inputs) {
  case_when(
    date %in% inputs$bank_holidays$date                ~ "bank_holiday",
    lubridate::wday(date, week_start = 1) >= 6         ~ "weekend",
    .default = "weekday"
  )
}
