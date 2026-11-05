# Reading and validating the rota input workbook and rotas.

yn_columns <- c("is_on_call", "is_night", "is_long", "is_late",
                "nights_exempt", "opted_out_wtr", "enabled")

# Columns holding codes, matched whatever their case or spacing ("Approved ")
code_columns <- list(leave = c("leave_type", "status"), requests = "request",
                     preferences = "preference")

# Columns that must be numbers. One text cell ("48 h") makes Excel's whole
# column text, which would compare alphabetically.
numeric_columns <- list(rules = c("value", "weight"), staff = c("wte", "contracted_hours"),
                        shifts = "hours", requests = "weight")

leave_statuses <- c("approved", "requested")

#' Read the rota input workbook
#'
#' Reads every sheet of the inputs workbook (staff, shifts, cover, rules,
#' leave, requests, ...) into a list of tibbles, ready for [check_rota()]
#' and, later, the solver. Dates become `Date`, `Y`/`N` columns become
#' logical, and times become `"HH:MM"` text even if Excel has turned them
#' into time values or they were typed as `8:00`. Codes such as a leave
#' status are read whatever their case or spacing (`"Approved "` is
#' `approved`). Cells that cannot be read become `NA`, and
#' [validate_inputs()] lists them.
#'
#' Run [validate_inputs()] afterwards to catch inconsistent data, such as
#' leave for a doctor who is not in the staff list.
#'
#' @param path Path to an `.xlsx` workbook. Which sheets it needs depends on
#'   the `rota_type` setting:
#'   * `resident` (the default): `settings`, `rules`, `grades`, `shifts`,
#'     `cover`, `bank_holidays`, `staff`, `placements`, `placement_roles`,
#'     `leave`, `requests` and `skill_mix`. See
#'     `rota_example("residents")`.
#'   * `consultant`: `settings`, `rules`, `shifts`, `cover`, `bank_holidays`,
#'     `staff` and `leave`. See
#'     `rota_example("icu-consultants")`.
#'
#'   Either may add `patterns`, `preferences`, `sequences`,
#'   `pattern_sequences` and `block_rest` sheets. Sheets left out are read as
#'   empty. A front sheet named `about`, describing the workbook for people,
#'   is skipped. Sheets may be in any order.
#'
#' @returns A `rota_inputs` object: a named list with one tibble per sheet
#'   (e.g. `inputs$staff`), plus `period`, a list holding the rota's `start`
#'   and `end` dates.
#'
#' @export
#' @examples
#' inputs <- read_rota_inputs(rota_example())
#' inputs
#' inputs$staff
read_rota_inputs <- function(path) {
  check_file(path, "inputs workbook")
  sheets <- setdiff(readxl::excel_sheets(path), "about")
  if (!"settings" %in% sheets) {
    stop("Missing sheets in ", path, ": settings", call. = FALSE)
  }

  inputs <- set_names(sheets) |>
    map(\(s) readxl::read_excel(path, sheet = s)) |>
    imap(tidy_sheet)
  read_problems <- imap(inputs, \(df, sheet) {
    tibble::tibble(sheet = sheet, problem = attr(df, "problems") %||% character())
  }) |>
    list_rbind()
  inputs <- map(inputs, \(df) `attr<-`(df, "problems", NULL))
  missing <- setdiff(rota_types[[rota_type(inputs$settings)]]$required, sheets)
  if (length(missing) > 0) {
    stop("Missing sheets in ", path, ": ", paste(missing, collapse = ", "), call. = FALSE)
  }
  inputs <- add_optional_inputs(inputs)

  setting <- set_names(inputs$settings$value, inputs$settings$key)
  inputs$period <- list(
    start = parse_date(setting[["rota_start"]]),
    end   = parse_date(setting[["rota_end"]])
  )
  structure(inputs, class = c("rota_inputs", "list"), read_problems = read_problems)
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
         "In a rota workbook this is the \"long\" sheet, not \"by_doctor\".", call. = FALSE)
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

# Tidies one sheet. Cells that cannot be read (a Y/N column holding "maybe",
# a number column holding "48 h") become NA, and are listed in the
# "problems" attribute for validate_inputs() to report.
tidy_sheet <- function(df, sheet = "") {
  problems <- character()
  # The first column (rule_id, doctor_id, shift_code, ...) says which row
  row_label <- function(bad) if (ncol(df) > 0) as.character(df[[1]][bad]) else ""
  df <- df |>
    mutate(
      across(any_of(c("date", "start_date", "end_date")), as.Date),
      across(any_of(c("start", "end")), as_hhmm),
      across(any_of(code_columns[[sheet]]), \(x) stringr::str_to_lower(stringr::str_trim(x)))
    )

  for (col in intersect(c(yn_columns, grep("^can_", names(df), value = TRUE)), names(df))) {
    parsed <- parse_yn(df[[col]])
    bad <- is.na(parsed) & !is_blank(df[[col]])
    problems <- c(problems, sprintf("%s %s: \"%s\" should be Y or N", row_label(bad), col,
                                    df[[col]][bad]))
    df[[col]] <- parsed
  }
  for (col in intersect(numeric_columns[[sheet]], names(df))) {
    if (is.numeric(df[[col]])) next
    parsed <- suppressWarnings(as.numeric(stringr::str_trim(df[[col]])))
    bad <- is.na(parsed) & !is_blank(df[[col]])
    problems <- c(problems, sprintf("%s %s: \"%s\" is not a number", row_label(bad), col,
                                    df[[col]][bad]))
    df[[col]] <- parsed
  }
  attr(df, "problems") <- problems
  df
}

is_blank <- function(x) is.na(x) | stringr::str_trim(as.character(x)) == ""

# Y/N, also read as yes/no or TRUE/FALSE in any case; blank is NA
parse_yn <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  x <- stringr::str_to_upper(stringr::str_trim(as.character(x)))
  case_when(x %in% c("Y", "YES", "TRUE") ~ TRUE,
            x %in% c("N", "NO", "FALSE") ~ FALSE,
            .default = NA)
}

# Settings are text, but a user may type a real date into Excel
parse_date <- function(x) {
  if (grepl("^[0-9]+(\\.0+)?$", x)) {
    return(as.Date(as.numeric(x), origin = "1899-12-30"))
  }
  as.Date(x)
}

# Times are "HH:MM" text, but Excel may have turned them into times, and
# people type "8:00" or "08:00:00". Anything else is left as typed, for
# validate_inputs() to report.
as_hhmm <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(format(x, "%H:%M", tz = "UTC"))
  }
  x <- stringr::str_trim(as.character(x))
  # A time in a column that also holds text arrives as a fraction of a day
  fraction <- suppressWarnings(as.numeric(x))
  is_fraction <- !is.na(fraction) & fraction >= 0 & fraction < 1
  minutes <- round(fraction[is_fraction] * 1440)
  x[is_fraction] <- sprintf("%02d:%02d", minutes %/% 60, minutes %% 60)

  parts <- stringr::str_match(x, "^(\\d{1,2}):(\\d{2})(?::00)?$")
  ok <- !is.na(parts[, 1])
  x[ok] <- sprintf("%02d:%s", as.integer(parts[ok, 2]), parts[ok, 3])
  x
}

# A valid "HH:MM" time of day
is_hhmm <- function(x) {
  !is.na(x) & grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", x)
}

#' Find an example workbook
#'
#' Returns the path to a file in one of the example rotas in `examples/`.
#' Each example has an `input` folder holding its inputs workbook and an
#' `output` folder holding a solved rota, its check report and calendars.
#' The examples come with the source folder, so load rota-writer with
#' `pkgload::load_all()` from there.
#'
#' @param name The example: `"residents"` (resident doctors in anaesthesia
#'   and ICU) or `"icu-consultants"`.
#' @param file The file within it, e.g. `"output/rota.xlsx"`.
#'
#' @returns The file's path.
#'
#' @export
#' @examples
#' rota_example()
#' rota_example("icu-consultants", "output/rota.xlsx")
rota_example <- function(name = "residents", file = "input/inputs.xlsx") {
  path <- system.file("examples", name, file, package = "rotawriter")
  if (path == "") {
    stop("Can't find examples/", name, "/", file, ". Load rota-writer from its source folder ",
         "with pkgload::load_all().", call. = FALSE)
  }
  path
}

#' Read a rota
#'
#' Reads a rota in long format: one row per doctor per working day. This is
#' the format [check_rota()] uses. A rota workbook also has `by_role` and
#' `by_doctor` sheets for people to read, but those are not read here.
#'
#' @param path Path to a rota workbook, such as
#'   `rota_example("residents", "output/rota.xlsx")`.
#' @param sheet Name of the sheet holding the long-format rota, with
#'   columns `doctor_id`, `date` and `code`. Other columns are dropped.
#'
#' @returns A tibble with columns `doctor_id` (text), `date` (`Date`) and
#'   `code` (a shift code such as `ICU1_N`, or a leave code such as `AL`).
#'   Blank rows are dropped.
#'
#' @export
#' @examples
#' read_rota(rota_example("residents", "output/rota.xlsx"))
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
#' * cells that could not be read: text in a number column (such as
#'   `"48 h"`), or a `Y`/`N` column holding something else
#' * doctor IDs in `placements`, `leave` or `requests` that are not in `staff`
#' * duplicate or blank doctor IDs, unknown grades, and WTE blank or outside
#'   0.1-1
#' * leave that ends before it starts or has no dates, a leave status other
#'   than `approved` or `requested`, an unknown leave type, and unknown
#'   request types
#' * rules listed twice, unknown rule IDs, and rules that are on but have no
#'   value (a blank value would otherwise switch a hard rule off without a
#'   word)
#' * shift times that are not times, and shifts whose `hours` do not match
#'   their start and end times
#' * unknown shift codes, roles or grades in `cover`, `placement_roles` and
#'   `skill_mix`
#' * in-post days with no placement
#' * unknown shift codes, patterns, weekdays or preference types in the
#'   shifts sheet's `covers` column and the `patterns`, `preferences`,
#'   `sequences`, `pattern_sequences` and `block_rest` sheets
#'
#' @inheritParams check_rota
#'
#' @returns A tibble with one row per problem: `sheet` (where to fix it) and
#'   `problem` (description). Zero rows means the inputs are consistent.
#'
#' @export
#' @examples
#' inputs <- read_rota_inputs(rota_example())
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
  good_times <- is_hhmm(shifts$start) & is_hhmm(shifts$end)
  clock_hours <- shift_clock_hours(shifts$start, shifts$end)
  skill_shifts <- unique(unlist(stringr::str_split(inputs$skill_mix$shifts, ";")))
  leave <- inputs$leave
  wte <- inputs$staff$wte

  list(
    attr(inputs, "read_problems"),
    problem("staff", anyDuplicated(ids) > 0, "Duplicate doctor_id in staff"),
    problem("staff", anyNA(ids), "A row has no doctor_id"),
    problem("staff", which(!is.na(inputs$staff$grade) & !inputs$staff$grade %in% inputs$grades$grade),
            paste("Unknown grade:", inputs$staff$grade)),
    problem("staff", which(is.na(wte) | !between(wte, 0.1, 1)),
            paste0("WTE should be between 0.1 and 1 for ", ids, " (it is ", wte, ")")),
    unknown_ids("placements"),
    unknown_ids("leave"),
    unknown_ids("requests"),
    problem("leave", which(is.na(leave$start_date) | is.na(leave$end_date)),
            paste("Leave has no start_date or end_date for", leave$doctor_id)),
    problem("leave", which(leave$end_date < leave$start_date),
            paste("Leave ends before it starts for", leave$doctor_id)),
    problem("leave", which(!leave$status %in% leave_statuses),
            paste0("Leave status for ", leave$doctor_id, " from ", leave$start_date, " is \"",
                   leave$status, "\": use approved or requested")),
    problem("leave", which(!leave$leave_type %in% names(leave_codes)),
            paste0("Leave type for ", leave$doctor_id, " from ", leave$start_date, " is \"",
                   leave$leave_type, "\": use ", paste(names(leave_codes), collapse = ", "))),
    problem("requests", which(!inputs$requests$request %in% request_types),
            paste0("Unknown request for ", inputs$requests$doctor_id, " on ", inputs$requests$date,
                   ": \"", inputs$requests$request, "\". Use ",
                   paste(request_types, collapse = ", "))),
    problem("requests", which(is.na(inputs$requests$date)),
            paste("Request has no date for", inputs$requests$doctor_id)),
    problem("shifts", which(!good_times),
            paste0(shifts$shift_code, ": start and end should be times like 08:00 (they are ",
                   shifts$start, " and ", shifts$end, ")")),
    problem("shifts", which(is.na(shifts$hours)), paste0(shifts$shift_code, ": hours is blank")),
    problem("shifts", which(good_times & abs(clock_hours - shifts$hours) > 0.01),
            paste0(shifts$shift_code, ": hours (", shifts$hours, ") do not match ",
                   shifts$start, "-", shifts$end)),
    validate_rules(inputs),
    problem("cover", which(!inputs$cover$shift_code %in% shifts$shift_code),
            paste("Unknown shift_code:", inputs$cover$shift_code)),
    problem("placement_roles", which(!inputs$placement_roles$role %in% shifts$role),
            paste("Unknown role:", inputs$placement_roles$role)),
    problem("skill_mix", which(!skill_shifts %in% shifts$shift_code),
            paste("Unknown shift_code:", skill_shifts)),
    problem("skill_mix", which(!inputs$skill_mix$min_grade %in% inputs$grades$grade),
            paste("Unknown grade:", inputs$skill_mix$min_grade)),
    if (rule_on(inputs, "PLACEMENT")) missing_placements(inputs),
    unknown_ids("preferences"),
    validate_slots_and_patterns(inputs)
  ) |>
    list_rbind()
}

# Each rule listed once, and a number for every rule that is on and needs one
validate_rules <- function(inputs) {
  rules <- inputs$rules
  dupes <- unique(rules$rule_id[duplicated(rules$rule_id)])
  unknown <- setdiff(rules$rule_id, known_rule_ids())
  value <- \(id) suppressWarnings(as.numeric(rules$value[match(id, rules$rule_id)]))
  needed <- value_rules[map_lgl(value_rules, \(id) rule_on(inputs, id))]
  if (rule_on(inputs, "AVG_HOURS") && any(inputs$staff$opted_out_wtr %in% TRUE)) {
    needed <- c(needed, "AVG_HOURS_OPTOUT")
  }
  if (rule_on(inputs, "HOURS_TARGET") && anyNA(inputs$staff$contracted_hours)) {
    needed <- c(needed, "HOURS_TARGET")
  }
  missing_row <- setdiff(needed, rules$rule_id)
  no_value <- intersect(needed, rules$rule_id)
  no_value <- no_value[is.na(map_dbl(no_value, value))]

  tibble::tibble(sheet = "rules", problem = c(
    sprintf("%s is listed more than once", dupes),
    sprintf("Unknown rule_id: %s", unknown),
    sprintf("%s is on but has no row: add it with its value", missing_row),
    sprintf("%s is on but its value is blank: add a number, or set enabled to N", no_value)
  ))
}

# The covers column, and the patterns, preferences, sequences, pattern_sequences
# and block_rest sheets
validate_slots_and_patterns <- function(inputs) {
  problem <- function(sheet, bad, msg) tibble::tibble(sheet = rep(sheet, length(bad)), problem = msg)
  codes <- inputs$shifts$shift_code
  slots <- shift_covers(inputs$shifts)$slot
  days <- pattern_days(inputs)
  prefs <- inputs$preferences
  pattern_ids <- unique(inputs$patterns$pattern_id)
  check_values <- function(type, allowed) {
    bad <- prefs$value[prefs$preference == type & !prefs$value %in% allowed]
    problem("preferences", bad, paste0("Unknown value for ", type, ": ", bad))
  }
  bad_types <- setdiff(prefs$preference, preference_types)
  bad_slots <- setdiff(slots, codes)
  bad_pattern_slots <- setdiff(days$slot, codes)
  bad_days <- unique(days$pattern_id[is.na(days$offset)])
  bad_seq <- setdiff(c(inputs$sequences$from_shift, inputs$sequences$to_shift), codes)
  bad_pattern_seq <- setdiff(c(inputs$pattern_sequences$from_pattern, inputs$pattern_sequences$to_pattern),
                             pattern_ids)
  bad_rest_patterns <- setdiff(inputs$block_rest$pattern_id, pattern_ids)
  rest_shifts <- stringr::str_trim(unlist(stringr::str_split(na.omit(as.character(inputs$block_rest$shifts)), ";")))
  bad_rest_shifts <- setdiff(rest_shifts, codes)

  bind_rows(
    problem("shifts", bad_slots, paste("Unknown shift_code in covers:", bad_slots)),
    problem("patterns", bad_pattern_slots, paste("Unknown slot:", bad_pattern_slots)),
    problem("patterns", bad_days, paste0(bad_days, ": days must be Mon, Tue, Wed, Thu, Fri, Sat or Sun ",
                                         "separated by ;")),
    problem("preferences", bad_types, paste("Unknown preference:", bad_types)),
    check_values("prefer_shift", codes),
    check_values("avoid_shift", codes),
    check_values("prefer_pattern", pattern_ids),
    check_values("avoid_pattern", pattern_ids),
    check_values("day_off", weekday_names),
    problem("sequences", bad_seq, paste("Unknown shift_code:", bad_seq)),
    problem("pattern_sequences", bad_pattern_seq, paste("Unknown pattern_id:", bad_pattern_seq)),
    problem("block_rest", bad_rest_patterns, paste("Unknown pattern_id:", bad_rest_patterns)),
    problem("block_rest", bad_rest_shifts, paste("Unknown shift_code:", bad_rest_shifts)),
    if (isTRUE(rota_types[[rota_type(inputs$settings)]]$search$whole_weeks) &&
        lubridate::wday(inputs$period$start, week_start = 1) != 1) {
      problem("settings", 1, "rota_start should be a Monday, so blocks fill whole weeks")
    }
  )
}

# A shift ending at its start time lasts 24 h
shift_clock_hours <- function(start, end) {
  hours <- (hhmm_hours(end) - hhmm_hours(start)) %% 24
  if_else(hours == 0, 24, hours)
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
