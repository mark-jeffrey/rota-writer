# Shared parts for scripts that write an inputs workbook.
#
#   source("data-raw/template_parts.R")
#
# Each make_*_data.R script sources this from the project root. A new rota
# template starts from rule_rows(), england_bank_holidays() and add_sheet()
# rather than copying them from another script.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(openxlsx2)
})

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

# Bank holidays -----------------------------------------------------------
# England and Wales, from gov.uk/bank-holidays. Scotland and Northern Ireland
# have different ones, and one-off holidays are announced at short notice:
# check the list for the rota's dates.

england_bank_holidays <- function(start, end) {
  tribble(
    ~date,              ~name,
    ymd("2026-01-01"),  "New Year's Day",
    ymd("2026-04-03"),  "Good Friday",
    ymd("2026-04-06"),  "Easter Monday",
    ymd("2026-05-04"),  "Early May bank holiday",
    ymd("2026-05-25"),  "Spring bank holiday",
    ymd("2026-08-31"),  "Summer bank holiday",
    ymd("2026-12-25"),  "Christmas Day",
    ymd("2026-12-28"),  "Boxing Day (substitute)",
    ymd("2027-01-01"),  "New Year's Day",
    ymd("2027-03-26"),  "Good Friday",
    ymd("2027-03-29"),  "Easter Monday",
    ymd("2027-05-03"),  "Early May bank holiday",
    ymd("2027-05-31"),  "Spring bank holiday",
    ymd("2027-08-30"),  "Summer bank holiday",
    ymd("2027-12-27"),  "Christmas Day (substitute)",
    ymd("2027-12-28"),  "Boxing Day (substitute)",
    ymd("2028-01-03"),  "New Year's Day (substitute)",
    ymd("2028-04-14"),  "Good Friday",
    ymd("2028-04-17"),  "Easter Monday",
    ymd("2028-05-01"),  "Early May bank holiday",
    ymd("2028-05-29"),  "Spring bank holiday",
    ymd("2028-08-28"),  "Summer bank holiday",
    ymd("2028-12-25"),  "Christmas Day",
    ymd("2028-12-26"),  "Boxing Day",
    ymd("2029-01-01"),  "New Year's Day",
    ymd("2029-03-30"),  "Good Friday",
    ymd("2029-04-02"),  "Easter Monday",
    ymd("2029-05-07"),  "Early May bank holiday",
    ymd("2029-05-28"),  "Spring bank holiday",
    ymd("2029-08-27"),  "Summer bank holiday",
    ymd("2029-12-25"),  "Christmas Day",
    ymd("2029-12-26"),  "Boxing Day"
  ) |>
    filter(between(date, start, end))
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
add_sheet <- function(wb, name, x, dropdowns = list()) {
  wb <- wb |>
    wb_add_worksheet(name) |>
    wb_add_data_table(name, x, table_style = "TableStyleMedium2", na.strings = "") |>
    wb_freeze_pane(name, first_row = TRUE) |>
    wb_set_col_widths(name, cols = seq_along(x), widths = "auto")
  for (col in names(dropdowns)) {
    j <- match(col, names(x))
    wb <- wb_add_data_validation(
      wb, name,
      dims = wb_dims(rows = 2:500, cols = j),
      type = "list",
      value = paste0('"', paste(dropdowns[[col]], collapse = ","), '"')
    )
  }
  wb
}
