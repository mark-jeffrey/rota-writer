# Shared parts for scripts that write an inputs workbook.
#
#   source("data-raw/template_parts.R")
#
# Each make_*_data.R script sources this from the project root. A new rota
# template starts from rule_rows(), uk_bank_holidays() and add_sheet()
# rather than copying them from another script.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(openxlsx2)
})

# rota-writer's exported functions, for uk_bank_holidays()
pkgload::load_all(quiet = TRUE, export_all = FALSE)

# Rules -------------------------------------------------------------------
# Every rule in rule_registry() (R/rules.R), with its usual description,
# value and weight. The hard contract rules come from the 2016 TCS Schedule 3
# (docs/contract-rules.md); the soft weights are those of the resident test
# workbook, ranked as: requested leave, then wellbeing patterns, then
# fairness, then hours, then tie-breakers. Rules after REQUESTS were added
# for the consultant rota.

rule_library <- tribble(
  ~rule_id,             ~description,                                                ~value, ~unit,      ~hard_soft, ~weight, ~enabled,
  "AVG_HOURS",          "Max average weekly hours over reference period (x WTE)",    48,     "hours",    "hard",     NA,      "Y",
  "AVG_HOURS_OPTOUT",   "Max average weekly hours if opted out of WTR (x WTE)",      56,     "hours",    "hard",     NA,      "Y",
  "MAX_7DAY",           "Max hours in any 168-hour period",                          72,     "hours",    "hard",     NA,      "Y",
  "MAX_SHIFT",          "Max shift length",                                          13,     "hours",    "hard",     NA,      "Y",
  "MIN_REST",           "Min rest between shifts",                                   11,     "hours",    "hard",     NA,      "Y",
  "MAX_CONSEC_SHIFTS",  "Max consecutive shifts (then 48h rest)",                    7,      "shifts",   "hard",     NA,      "Y",
  "MAX_CONSEC_LONG",    "Max consecutive long shifts >10h (then 48h rest)",          4,      "shifts",   "hard",     NA,      "Y",
  "MAX_CONSEC_LATE",    "Max consecutive long evening shifts (then 48h rest)",       4,      "shifts",   "hard",     NA,      "Y",
  "REST_AFTER_RUN",     "Rest after max consecutive shift/long/late runs",           48,     "hours",    "hard",     NA,      "Y",
  "MAX_CONSEC_NIGHTS",  "Max consecutive night shifts",                              4,      "shifts",   "hard",     NA,      "Y",
  "REST_AFTER_NIGHTS",  "Rest after any run of nights",                              46,     "hours",    "hard",     NA,      "Y",
  "WEEKEND_MAX",        "Weekend frequency hard limit (1 in N)",                     2,      "1 in N",   "hard",     NA,      "Y",
  "WEEKEND_TARGET",     "Weekend frequency target (1 in N)",                         3,      "1 in N",   "soft",     50,      "Y",
  "LTFT_FIXED_DAY",     "No work on LTFT fixed non-working day",                     NA,     "",         "hard",     NA,      "Y",
  "COVER",              "All on-call slots filled; day cover minimum met",           NA,     "",         "hard",     NA,      "Y",
  "IN_POST",            "Only rostered between staff start_date and end_date",       NA,     "",         "hard",     NA,      "Y",
  "COMPETENCY",         "Only work roles signed off for (staff can_* columns)",      NA,     "",         "hard",     NA,      "Y",
  "PLACEMENT",          "Only work roles listed for current placement",              NA,     "",         "hard",     NA,      "Y",
  "PLACEMENT_AVOID",    "Discouraged placement roles (penalty per shift in placement_roles)", NA, "",  "soft",     1,       "Y",
  "NIGHTS_EXEMPT",      "Nights-exempt doctors never rostered nights",               NA,     "",         "hard",     NA,      "Y",
  "LEAVE_APPROVED",     "Approved leave honoured",                                   NA,     "",         "hard",     NA,      "Y",
  "LEAVE_REQUESTED",    "Requested leave honoured (penalty per day not granted)",    NA,     "",         "soft",     500,     "Y",
  "REQUESTS",           "Preference requests (weight per row in requests sheet)",    NA,     "",         "soft",     1,       "Y",
  "SKILL_MIX",          "Skill mix rules (weight per row in skill_mix sheet)",       NA,     "",         "soft",     1,       "Y",
  "FAIR_ONCALL",        "On-call shifts close to WTE-weighted share",                NA,     "",         "soft",     10,      "Y",
  "FAIR_NIGHTS",        "Nights close to WTE-weighted share",                        NA,     "",         "soft",     10,      "Y",
  "FAIR_WEEKENDS",      "Weekends close to WTE-weighted share",                      NA,     "",         "soft",     10,      "Y",
  "SINGLE_NIGHT",       "Avoid isolated single nights",                              NA,     "",         "soft",     100,     "Y",
  "LONG_RUN",           "Consecutive shifts target (weight per shift beyond it)",    5,      "shifts",   "soft",     150,     "Y",
  "LONG_NIGHT_RUN",     "Consecutive nights target (weight per night beyond it)",    2,      "shifts",   "soft",     50,      "N",
  "ICU_SHORT_RUN",      "ICU day-shift runs at least this long, for continuity (weight per day short)", 3, "shifts", "soft", 5, "Y",
  "ISOLATED_DAY_OFF",   "Avoid single days off between shifts",                      NA,     "",         "soft",     30,      "Y",
  "SPLIT_WEEKEND",      "Avoid working only one day of a weekend",                   NA,     "",         "soft",     150,      "Y",
  "HOURS_TARGET",       "Average weekly hours close to contracted (value: default hours x WTE if staff contracted_hours blank; weight per hour short)", 45, "hours", "soft", 5, "Y",
  "NORMAL_DAY_SPREAD",  "Normal-day staffing even across the rota (weight per doctor-day from the average)", NA, "", "soft", 10, "Y",
  "SEQUENCE",           "Shift sequences in the sequences sheet with no weight",     NA,     "",         "hard",     NA,      "Y",
  "SEQUENCE_SOFT",      "Shift sequences in the sequences sheet with a weight (weight per row)", NA, "", "soft",     1,       "Y",
  "PREFERENCE_ONLY",    "Shifts and patterns open only to consultants who prefer them", NA,  "",         "hard",     NA,      "Y",
  "PATTERNS",           "1st on and nights in the blocks in the patterns sheet (per day outside a block)", NA, "", "soft", 100, "Y",
  "PATTERN_UNPREFERRED", "A preference-only pattern (7-day 1st on) worked in full without preferring it (per week)", NA, "", "soft", 100, "Y",
  "PATTERN_SEQUENCE",   "Patterns back to back across two weeks in the pattern_sequences sheet (weight per row)", NA, "", "soft", 1, "Y",
  "BLOCK_REST",         "Rest before and after the big blocks in the block_rest sheet (per shift in the rest)", NA, "", "soft", 1000, "Y",
  "CONSEC_NIGHT_WEEKS", "Nights in two Mon-Sun weeks running (per pair of weeks)",   NA,     "",         "soft",     500,     "Y",
  "FAIR_SLOTS",         "1st on, 2nd on and nights close to WTE-weighted share",     NA,     "",         "soft",     10,      "Y",
  "PREFERENCES",        "Standing preferences (weight per row in preferences sheet)", NA,    "",         "soft",     1,       "Y"
)

# Rows of rule_library for a rules sheet, in the order given. Named
# arguments change a rule's columns, e.g.
#   rule_rows(c("COVER", "MIN_REST", "WEEKEND_MAX"), WEEKEND_MAX = list(value = 3))
rule_rows <- function(ids, ...) {
  changes <- list(...)
  unknown <- setdiff(c(ids, names(changes)), rule_library$rule_id)
  if (length(unknown) > 0) stop("Not in rule_library: ", paste(unknown, collapse = ", "))
  rows <- rule_library[match(ids, rule_library$rule_id), ]
  for (id in names(changes)) {
    for (col in names(changes[[id]])) {
      rows[[col]][rows$rule_id == id] <- changes[[id]][[col]]
    }
  }
  rows
}

# Drop-down values --------------------------------------------------------

yes_no <- c("Y", "N")
day_types <- c("weekday", "weekend", "bank_holiday")
shift_types <- c("normal_day", "long_day", "night")
leave_types <- c("annual", "study", "exam", "parental")
leave_statuses <- c("approved", "requested")
request_types <- c("no_oncall", "no_night", "no_weekend", "prefer_oncall")
preference_types <- c("prefer_shift", "avoid_shift", "day_off", "prefer_pattern", "avoid_pattern")
open_to_values <- c("all", "preference")

# Workbook ----------------------------------------------------------------

# Add a table sheet with a frozen header and drop-downs for the named columns
add_sheet <- function(wb, name, x, dropdowns = list(), table_style = "TableStyleMedium2",
                      tab_color = NULL) {
  wb <- wb |>
    wb_add_worksheet(name, tab_color = tab_color) |>
    wb_add_data_table(name, x, table_style = table_style, na.strings = "") |>
    wb_freeze_pane(name, first_row = TRUE) |>
    wb_set_col_widths(name, cols = seq_along(x), widths = "auto")
  for (col in intersect(names(dropdowns), names(x))) {
    wb <- wb_add_data_validation(
      wb, name,
      dims = wb_dims(rows = 2:1000, cols = match(col, names(x))),
      type = "list",
      value = paste0('"', paste(dropdowns[[col]], collapse = ","), '"')
    )
  }
  wb
}

# Inputs workbook ---------------------------------------------------------
# Sheets are grouped by who changes them and when, so a rota coordinator
# finds the ones they edit each rota period first. Each group has its own
# tab and header colour.

sheet_groups <- tribble(
  ~group,  ~label,               ~color,     ~table_style,        ~meaning,
  "edit",  "Edit each rota",     "FF70AD47", "TableStyleMedium7", "Change these for each rota period: dates, staff, placements, leave and requests.",
  "rules", "Rules and weights",  "FFED7D31", "TableStyleMedium3", "The rules and how much each soft rule matters. Tune the value, weight and enabled columns with care.",
  "setup", "Set up once",        "FF7F7F7F", "TableStyleMedium4", "How the rota is built: shifts, cover and blocks. Set up with the rule set and rarely changed."
)

# Every sheet an inputs workbook may have, in workbook order
sheet_guide <- tribble(
  ~sheet,              ~group,  ~purpose,
  "settings",          "edit",  "Rota type, start and end dates, and how long the solver runs",
  "staff",             "edit",  "One row per doctor: grade, WTE, dates in post, exemptions and competencies",
  "placements",        "edit",  "Each doctor's training placements, with dates",
  "leave",             "edit",  "Leave: approved leave is always honoured, requested leave if possible",
  "requests",          "edit",  "One-off requests for a date, such as no night or no on-call",
  "preferences",       "edit",  "Standing preferences, such as a usual day off or a preferred shift",
  "rules",             "rules", "Every rule: its limit, whether it is hard or soft, its weight and whether it is on",
  "grades",            "setup", "Grades in order of seniority, for the skill mix rules",
  "shifts",            "setup", "Every shift: role, type, start and end times and hours",
  "cover",             "setup", "How many doctors each shift needs on weekdays, weekends and bank holidays",
  "placement_roles",   "setup", "The roles each placement may work",
  "skill_mix",         "setup", "Seniority needed across groups of shifts",
  "patterns",          "setup", "Blocks a shift is worked in within a week, such as Mon-Thu or Fri-Sun",
  "sequences",         "setup", "Shifts that may not (or should not) follow other shifts",
  "pattern_sequences", "setup", "Blocks that should not be worked back to back across two weeks",
  "block_rest",        "setup", "Hours off before and after big blocks",
  "bank_holidays",     "setup", "Bank holidays in the rota period"
)

# Hover notes for column headers. A row with sheet "*" applies to every
# sheet without its own note for that column; "can_*" covers every
# competency column.
column_notes <- tribble(
  ~sheet,              ~column,              ~note,
  "*",                 "notes",              "Free text for people. Not read by the solver.",
  "*",                 "doctor_id",          "The doctor's ID, as in the staff sheet.",
  "*",                 "start_date",         "First day (dd/mm/yyyy).",
  "*",                 "end_date",           "Last day (dd/mm/yyyy).",
  "*",                 "date",               "The date (dd/mm/yyyy).",
  "*",                 "weight",             "Penalty when this row is not met. Higher matters more.",
  "*",                 "description",        "What this row is, in words.",
  "*",                 "shift_code",         "A shift code from the shifts sheet.",
  "*",                 "pattern_id",         "A pattern from the patterns sheet.",
  "*",                 "open_to",            "all: anyone may work it. preference: only doctors with a matching preference row.",
  "settings",          "key",                "The setting's name. Don't change.",
  "settings",          "value",              "The setting. Dates as yyyy-mm-dd.",
  "rules",             "rule_id",            "The rule's ID. Don't change: the solver looks rules up by it.",
  "rules",             "value",              "The rule's limit or target, in the unit shown. Change to tune.",
  "rules",             "unit",               "Unit of the value.",
  "rules",             "hard_soft",          "hard: never broken. soft: broken only if needed, at a penalty of the weight.",
  "rules",             "weight",             "Penalty each time a soft rule is broken (per the unit in the description). Higher matters more.",
  "rules",             "enabled",            "Y: the rule applies. N: it is switched off.",
  "grades",            "grade",              "Grade name, as used in the staff sheet.",
  "grades",            "rank",               "Seniority: higher is more senior.",
  "shifts",            "role",               "The job the shift does. Competencies (staff can_*) and placements are by role.",
  "shifts",            "shift_type",         "normal_day, long_day or night.",
  "shifts",            "start",              "Start time, HH:MM.",
  "shifts",            "end",                "End time, HH:MM. Earlier than the start means the next day.",
  "shifts",            "hours",              "Paid hours. Must match the start and end times.",
  "shifts",            "is_on_call",         "Y: an on-call shift, filled exactly and counted for fairness.",
  "shifts",            "is_night",           "Y: a night shift, for the night rules.",
  "shifts",            "is_long",            "Y: a long shift (over 10 hours), for the consecutive long shift rule.",
  "shifts",            "is_late",            "Y: a long evening shift, for the consecutive late shift rule.",
  "shifts",            "covers",             "Cover slots this shift fills, separated by ;. Blank: its own code.",
  "shifts",            "label",              "The shift's name in the rota key and calendars.",
  "shifts",            "area",               "Where the shift works, e.g. ICU or Theatres. The rota groups and colours shifts by area. Blank: its role.",
  "cover",             "day_type",           "weekday, weekend or bank_holiday.",
  "cover",             "n_min",              "Fewest doctors needed.",
  "cover",             "n_max",              "Most doctors allowed. Blank: no limit. Equal to n_min for on-call.",
  "bank_holidays",     "name",               "The bank holiday's name.",
  "staff",             "doctor_id",          "A short unique ID, used in every other sheet.",
  "staff",             "name",               "The doctor's name, as shown on the rota.",
  "staff",             "grade",              "Grade, from the grades sheet.",
  "staff",             "pool",               "Staffing pool. Fair shares are worked out within each pool. Blank: one pool.",
  "staff",             "wte",                "Whole-time equivalent, 0.1 to 1 (0.6 = 60%). Fair shares and hours scale with it.",
  "staff",             "contracted_hours",   "Average weekly hours to aim for. Blank: the HOURS_TARGET rule's value x WTE.",
  "staff",             "ltft_fixed_day_off", "Less than full time: a weekday never worked (Mon to Fri). Blank: none.",
  "staff",             "start_date",         "First day in post (dd/mm/yyyy).",
  "staff",             "end_date",           "Last day in post (dd/mm/yyyy).",
  "staff",             "nights_exempt",      "Y: never rostered nights.",
  "staff",             "opted_out_wtr",      "Y: opted out of the Working Time Regulations 48-hour average.",
  "staff",             "other_department",   "Where the rest of their time goes. For people; not read by the solver.",
  "staff",             "can_*",              "Y: signed off to work this role.",
  "placements",        "placement",          "The placement, as in the placement_roles sheet.",
  "placement_roles",   "placement",          "The placement.",
  "placement_roles",   "role",               "A role (shifts sheet) this placement may work. Roles not listed are not allowed.",
  "placement_roles",   "penalty",            "0: allowed. Above 0: allowed but discouraged, at this penalty per shift.",
  "leave",             "leave_type",         "annual, study, exam or parental.",
  "leave",             "status",             "approved: always honoured. requested: honoured if possible.",
  "requests",          "request",            "no_oncall, no_night, no_weekend or prefer_oncall.",
  "preferences",       "preference",         "prefer_shift, avoid_shift, day_off, prefer_pattern or avoid_pattern.",
  "preferences",       "value",              "A shift code, a pattern_id, or a weekday (Mon to Sun) for day_off.",
  "skill_mix",         "rule_id",            "A short ID for this skill mix rule.",
  "skill_mix",         "shifts",             "Shift codes counted together, separated by ;.",
  "skill_mix",         "min_grade",          "Lowest grade that counts, from the grades sheet.",
  "skill_mix",         "min_count",          "How many doctors of at least min_grade are needed across the shifts.",
  "patterns",          "pattern_id",         "A short ID for this block.",
  "patterns",          "slot",               "The shift code the block covers.",
  "patterns",          "days",               "Days of the Mon-Sun week in the block, separated by ;.",
  "sequences",         "from_shift",         "A shift code.",
  "sequences",         "to_shift",           "The shift code that may not follow it.",
  "sequences",         "days_after",         "Days after from_shift: 1 is the next day.",
  "sequences",         "weight",             "Blank: never allowed (hard). A number: allowed at this penalty.",
  "pattern_sequences", "from_pattern",       "The pattern worked in full in one week.",
  "pattern_sequences", "to_pattern",         "The pattern worked in full the week after.",
  "block_rest",        "shifts",             "Shift codes that make the block, separated by ;. Blank: any shift in its slot.",
  "block_rest",        "rest_before_h",      "Hours off before the block.",
  "block_rest",        "rest_after_h",       "Hours off after the block."
)

# Drop-downs every inputs workbook gets, by column name
standard_dropdowns <- list(
  hard_soft = c("hard", "soft"), enabled = yes_no, shift_type = shift_types,
  is_on_call = yes_no, is_night = yes_no, is_long = yes_no, is_late = yes_no,
  open_to = open_to_values, day_type = day_types, nights_exempt = yes_no,
  opted_out_wtr = yes_no, ltft_fixed_day_off = c("Mon", "Tue", "Wed", "Thu", "Fri"),
  leave_type = leave_types, status = leave_statuses, request = request_types,
  preference = preference_types
)

# Drop-downs that list another sheet's column, so a typo can't get in and a
# new doctor, grade, shift or pattern appears as soon as it is added
linked_dropdowns <- tribble(
  ~sheet,              ~column,        ~from_sheet, ~from_column,
  "placements",        "doctor_id",    "staff",     "doctor_id",
  "leave",             "doctor_id",    "staff",     "doctor_id",
  "requests",          "doctor_id",    "staff",     "doctor_id",
  "preferences",       "doctor_id",    "staff",     "doctor_id",
  "staff",             "grade",        "grades",    "grade",
  "skill_mix",         "min_grade",    "grades",    "grade",
  "cover",             "shift_code",   "shifts",    "shift_code",
  "patterns",          "slot",         "shifts",    "shift_code",
  "sequences",         "from_shift",   "shifts",    "shift_code",
  "sequences",         "to_shift",     "shifts",    "shift_code",
  "pattern_sequences", "from_pattern", "patterns",  "pattern_id",
  "pattern_sequences", "to_pattern",   "patterns",  "pattern_id",
  "block_rest",        "pattern_id",   "patterns",  "pattern_id"
)

# Columns the rules sheet is tuned by, shaded so they stand out
rules_tunable <- c("value", "weight", "enabled")

date_columns <- c("date", "start_date", "end_date")

column_note <- function(sheet, column) {
  key <- if (startsWith(column, "can_")) "can_*" else column
  notes <- column_notes |> filter(column == key, sheet %in% c(.env$sheet, "*"))
  if (nrow(notes) == 0) return(NA_character_)
  notes$note[order(notes$sheet == "*")][1]
}

# Write an inputs workbook: a front "about" sheet, then the sheets in
# sheet_guide order, coloured by group, with header notes and drop-downs.
#   sheets:      named list of tibbles, one per sheet
#   dropdowns:   named list of extra drop-downs per sheet, e.g.
#                list(placements = list(placement = c("ICU", "OBS")))
#   title:       the workbook's name, for the about sheet
#   description: a few sentences on what the rota is
write_inputs_workbook <- function(path, sheets, title, description, dropdowns = list(),
                                  created = as.POSIXct("2026-10-31 12:00:00", tz = "UTC")) {
  unknown <- setdiff(names(sheets), sheet_guide$sheet)
  if (length(unknown) > 0) stop("Not in sheet_guide: ", paste(unknown, collapse = ", "))
  guide <- sheet_guide |>
    filter(sheet %in% names(sheets)) |>
    left_join(sheet_groups, by = "group")

  withr::local_options(openxlsx2.dateFormat = "dd/mm/yyyy")
  wb <- wb_workbook(creator = "rota-writer", datetime_created = created) |>
    add_about_sheet(guide, title, description)

  for (i in seq_len(nrow(guide))) {
    name <- guide$sheet[i]
    x <- sheets[[name]]
    yn <- names(x)[startsWith(names(x), "can_")]
    wb <- wb |>
      add_sheet(name, x, c(dropdowns[[name]], standard_dropdowns, set_names(rep(list(yes_no), length(yn)), yn)),
                table_style = guide$table_style[i], tab_color = wb_color(guide$color[i])) |>
      wb_add_fill(name, dims = wb_dims(rows = 1, cols = seq_along(x)), color = wb_color(guide$color[i])) |>
      wb_add_font(name, dims = wb_dims(rows = 1, cols = seq_along(x)), bold = TRUE,
                  color = wb_color("FFFFFFFF"))
    for (j in seq_along(x)) {
      note <- column_note(name, names(x)[j])
      if (is.na(note)) {
        message("No header note for ", name, "$", names(x)[j], ": add one to column_notes")
        next
      }
      wb <- wb_add_comment(wb, name, dims = wb_dims(rows = 1, cols = j),
                           comment = wb_comment(note, author = "rota-writer", width = 3, height = 3))
    }
    if (name == "rules") {
      tunable <- match(intersect(rules_tunable, names(x)), names(x))
      for (j in tunable) {
        wb <- wb_add_fill(wb, name, dims = wb_dims(rows = seq_len(nrow(x)) + 1, cols = j),
                          color = wb_color("FFFCE4D6"))
      }
    }
    for (j in which(names(x) %in% date_columns)) {
      wb <- wb_add_data_validation(
        wb, name, dims = wb_dims(rows = 2:1000, cols = j), type = "date",
        operator = "between", value = as.Date(c("2000-01-01", "2100-12-31")),
        error_title = "Not a date", error = "Type a date, e.g. 01/02/2027."
      )
    }
    links <- linked_dropdowns |>
      filter(sheet == name, column %in% names(x), from_sheet %in% names(sheets))
    for (k in seq_len(nrow(links))) {
      from <- sheets[[links$from_sheet[k]]]
      col <- int2col(match(links$from_column[k], names(from)))
      wb <- wb_add_data_validation(
        wb, name, dims = wb_dims(rows = 2:1000, cols = match(links$column[k], names(x))),
        type = "list", value = sprintf("%s!$%s$2:$%s$1000", links$from_sheet[k], col, col),
        error_title = "Not in the list",
        error = paste0("Use a ", links$from_column[k], " from the ", links$from_sheet[k], " sheet.")
      )
    }
  }
  wb_save(wb, path, overwrite = TRUE)
}

# The front sheet: what the workbook is, how to use it, and a key to its
# sheets. read_rota_inputs() skips it.
add_about_sheet <- function(wb, guide, title, description) {
  how_to <- c(
    "How to use this workbook",
    "1. Edit the green sheets for each rota period: dates in settings, then staff, leave and requests.",
    "2. Leave the orange rules sheet as it is to begin with. Change a soft rule's weight to make it matter more or less.",
    "3. The grey sheets describe how the rota is built. They are set up once, with the rule set.",
    "4. Hover over a column heading for what goes in it. Drop-downs list the allowed values.",
    "5. Solve in the app (Rscript scripts/run_app.R) or with Rscript scripts/solve_rota.R inputs.xlsx out/",
    "6. Re-check a rota edited by hand with Rscript scripts/check_rota.R inputs.xlsx out/rota.xlsx out/report.xlsx"
  )
  key <- guide |> transmute(Sheet = sheet, Group = label, Purpose = purpose)
  colour_row <- 4 + length(how_to) + 1
  key_row <- colour_row + nrow(sheet_groups) + 3

  wb <- wb |>
    wb_add_worksheet("about", tab_color = wb_color("FF2F5597")) |>
    wb_add_data("about", title, dims = "A1") |>
    wb_add_font("about", dims = "A1", bold = TRUE, size = 16) |>
    wb_add_data("about", description, dims = "A2") |>
    wb_add_data("about", how_to, dims = wb_dims(rows = 4, cols = 1), col_names = FALSE) |>
    wb_add_data("about", "Colours", dims = wb_dims(rows = colour_row, cols = 1)) |>
    wb_add_data("about", select(sheet_groups, label, meaning),
                dims = wb_dims(rows = colour_row + 1, cols = 1), col_names = FALSE) |>
    wb_add_data("about", "Sheets", dims = wb_dims(rows = key_row - 1, cols = 1)) |>
    wb_add_data("about", key, dims = wb_dims(rows = key_row, cols = 1)) |>
    wb_set_col_widths("about", cols = 1:3, widths = c(20, 20, 90))
  for (row in c(4, colour_row, key_row - 1, key_row)) {
    wb <- wb_add_font(wb, "about", dims = wb_dims(rows = row, cols = 1:3), bold = TRUE)
  }
  fills <- tibble(row = c(colour_row + seq_len(nrow(sheet_groups)), key_row + seq_len(nrow(guide))),
                  cols = c(rep(list(1), nrow(sheet_groups)), rep(list(1:2), nrow(guide))),
                  color = c(sheet_groups$color, guide$color))
  for (i in seq_len(nrow(fills))) {
    dims <- wb_dims(rows = fills$row[i], cols = fills$cols[[i]])
    wb <- wb |>
      wb_add_fill("about", dims = dims, color = wb_color(fills$color[i])) |>
      wb_add_font("about", dims = dims, color = wb_color("FFFFFFFF"))
  }
  wb |> wb_set_grid_lines("about", show = FALSE)
}
