# Generate the ICU consultant inputs workbook for rota-writer.
#
#   Rscript data-raw/make_consultant_data.R
#
# Writes examples/icu-consultants/input/inputs.xlsx: a year (52 weeks) of 1st-on,
# 2nd-on and night cover for 15 consultants. Deterministic: fixed seed and
# fixed workbook timestamp. All names are fictional.

source("data-raw/template_parts.R")

set.seed(20270201)
inputs_path <- "examples/icu-consultants/input/inputs.xlsx"

rota_start <- ymd("2027-02-01") # Monday
rota_weeks <- 52
rota_end <- rota_start + weeks(rota_weeks) - days(1) # Sunday
mondays <- rota_start + weeks(0:(rota_weeks - 1))

# Settings ----------------------------------------------------------------

settings <- tribble(
  ~key,                    ~value,               ~notes,
  "rota_type",             "consultant",         "consultant: only the rules listed below apply",
  "rota_start",            format(rota_start),   "First day of rota (Monday)",
  "rota_end",              format(rota_end),     "Last day of rota (Sunday)",
  "department",            "ICU consultants (test)", "",
  "solver",                "highs",              "",
  "solver_time_limit_s",   "1800",               "Stop and return best rota found after this",
  "neighbourhood_days",    "28",                 "Days the search re-solves at a time (whole weeks)",
  "neighbourhood_doctors", "5",                  "Consultants the search re-solves at a time"
)

# Rules -------------------------------------------------------------------
# Consultants are not on the resident doctors' TCS: only the rules listed
# here apply. Weights follow the order: requested leave, then blocks, then
# fairness, then individual preferences. Rest around big blocks is almost
# hard, and nights in weeks running come close behind requested leave.

rules <- rule_rows(
  c("COVER", "IN_POST", "LEAVE_APPROVED", "MIN_REST", "WEEKEND_MAX", "SEQUENCE", "PREFERENCE_ONLY",
    "LEAVE_REQUESTED", "BLOCK_REST", "CONSEC_NIGHT_WEEKS", "PATTERNS", "PATTERN_UNPREFERRED",
    "PATTERN_SEQUENCE", "FAIR_SLOTS", "FAIR_WEEKENDS", "SPLIT_WEEKEND", "PREFERENCES", "REQUESTS"),
  COVER = list(description = "1st on, 2nd on and night filled every day"),
  MIN_REST = list(description = "Min rest between shifts: off the day after a night or 24 h"),
  WEEKEND_MAX = list(value = 3),
  SPLIT_WEEKEND = list(weight = 20)
)

# Shifts and cover --------------------------------------------------------
# The night is on site 17:00-22:00, then on call from home until 08:00. A
# 24-hour shift is the 2nd-on day running into the night: it fills both
# slots, and is open only to consultants who prefer it.

shifts <- tribble(
  ~shift_code, ~role,    ~area,    ~shift_type, ~start,  ~end,    ~hours, ~covers,      ~open_to,     ~label,
  "1ST",       "FIRST",  "1st on", "long_day",  "08:00", "18:00", 10,     NA,           "all",        "1st on",
  "2ND",       "SECOND", "2nd on", "long_day",  "08:00", "18:00", 10,     NA,           "all",        "2nd on",
  "NIGHT",     "NIGHT",  "Nights", "night",     "17:00", "08:00", 15,     NA,           "all",        "Night (on site to 22:00)",
  "24H",       "SECOND", "2nd on", "night",     "08:00", "08:00", 24,     "2ND;NIGHT",  "preference", "2nd on into the night"
) |>
  mutate(is_on_call = "Y", is_night = if_else(shift_type == "night", "Y", "N"), is_long = "N",
         is_late = "N", .after = hours)

cover <- crossing(shift_code = c("1ST", "2ND", "NIGHT"),
                  day_type = c("weekday", "weekend", "bank_holiday")) |>
  mutate(n_min = 1, n_max = 1)

bank_holidays <- uk_bank_holidays(rota_start, rota_end, nation = "england-and-wales")

# Blocks: 1st on Mon-Thu and Fri-Sun, or the whole week for those who prefer
# it; nights split between two consultants each week
patterns <- tribble(
  ~pattern_id, ~slot,   ~days,                           ~open_to,
  "1ST_MT",    "1ST",   "Mon;Tue;Wed;Thu",               "all",
  "1ST_FS",    "1ST",   "Fri;Sat;Sun",                   "all",
  "1ST_7D",    "1ST",   "Mon;Tue;Wed;Thu;Fri;Sat;Sun",   "preference",
  "N_MWFS",    "NIGHT", "Mon;Wed;Fri;Sun",               "all",
  "N_TTS",     "NIGHT", "Tue;Thu;Sat",                   "all"
)

# Never 1st on the day before a night
sequences <- tribble(
  ~from_shift, ~to_shift, ~days_after, ~weight,
  "1ST",       "NIGHT",   1,           NA,
  "1ST",       "24H",     1,           NA
)

# Not 1st on Fri-Sun then Mon-Thu: 7 days running across the weekend
pattern_sequences <- tribble(
  ~from_pattern, ~to_pattern, ~weight, ~notes,
  "1ST_FS",      "1ST_MT",    500,     "7 days 1st on across the weekend"
)

# 48 hours off before and after a 7-day 1st-on week, and a night block
# worked all as 24-hour shifts
block_rest <- tribble(
  ~pattern_id, ~shifts, ~rest_before_h, ~rest_after_h, ~notes,
  "1ST_7D",    "1ST",   48,             48,            "7 days 1st on",
  "N_MWFS",    "24H",   48,             48,            "A week of 24-hour shifts",
  "N_TTS",     "24H",   48,             48,            "A week of 24-hour shifts"
)

# Staff -------------------------------------------------------------------
# Most consultants give a quarter of their time to another department, so
# their ICU WTE is 0.75.

staff <- tribble(
  ~doctor_id, ~name,               ~wte, ~other_department,
  "C01",      "Dr Alys Avon",      1.00, NA,
  "C02",      "Dr Bryn Bure",      0.75, "Emergency medicine",
  "C03",      "Dr Cleo Calder",    0.75, "Anaesthesia",
  "C04",      "Dr Dara Dee",       0.75, "Emergency medicine",
  "C05",      "Dr Emyr Esk",       0.75, "Respiratory medicine",
  "C06",      "Dr Fion Fal",       0.75, "Respiratory medicine",
  "C07",      "Dr Gwen Glaslyn",   1.00, NA,
  "C08",      "Dr Huw Hodder",     0.75, "Anaesthesia",
  "C09",      "Dr Isla Irwell",    0.75, "Emergency medicine",
  "C10",      "Dr Joss Jed",       0.75, "Anaesthesia",
  "C11",      "Dr Kezia Kennet",   0.75, "Respiratory medicine",
  "C12",      "Dr Luca Lune",      0.75, "Emergency medicine",
  "C13",      "Dr Mali Medway",    1.00, NA,
  "C14",      "Dr Nye Nidd",       0.75, "Anaesthesia",
  "C15",      "Dr Orla Ouse",      0.75, "Respiratory medicine"
) |>
  mutate(start_date = rota_start, end_date = rota_end, .after = wte)

# Leave -------------------------------------------------------------------
# Six weeks of annual leave each, in whole weeks, plus a few study days. At
# most two consultants are away in any week. A fifth of the annual leave is
# still only requested.

away <- integer(rota_weeks)
annual <- map(staff$doctor_id, \(id) {
  picked <- integer()
  for (i in 1:6) {
    open <- setdiff(which(away < 2), picked)
    week <- if (length(open) > 1) sample(open, 1) else open
    away[week] <<- away[week] + 1
    picked <- c(picked, week)
  }
  tibble(doctor_id = id, start_date = mondays[sort(picked)])
}) |>
  list_rbind() |>
  mutate(end_date = start_date + 6, leave_type = "annual",
         status = if_else(runif(n()) < 0.2, "requested", "approved"))

study <- staff |>
  select(doctor_id) |>
  mutate(start_date = mondays[sample(seq_along(mondays), n(), replace = TRUE)] + sample(0:3, n(), replace = TRUE),
         end_date = start_date + 1, leave_type = "study", status = "approved") |>
  # Not in the middle of annual leave
  anti_join(annual, join_by(doctor_id, overlaps(start_date, end_date, start_date, end_date)))

leave <- bind_rows(annual, study) |>
  arrange(doctor_id, start_date) |>
  mutate(notes = NA_character_)

# Preferences and requests --------------------------------------------------

preferences <- tribble(
  ~doctor_id, ~preference,      ~value,   ~weight, ~notes,
  "C03",      "prefer_pattern", "1ST_7D", 50,      "Prefers a 7-day 1st-on week",
  "C08",      "prefer_pattern", "1ST_7D", 50,      "Prefers a 7-day 1st-on week",
  "C02",      "prefer_shift",   "24H",    20,      "Prefers 24-hour on-calls",
  "C05",      "prefer_shift",   "24H",    20,      "Prefers 24-hour on-calls",
  "C11",      "prefer_shift",   "24H",    20,      "Prefers 24-hour on-calls",
  "C14",      "prefer_shift",   "24H",    20,      "Prefers 24-hour on-calls",
  "C04",      "day_off",        "Tue",    5,       "Emergency department shift",
  "C06",      "day_off",        "Thu",    5,       "Respiratory clinic",
  "C09",      "day_off",        "Wed",    5,       "Emergency department shift",
  "C12",      "day_off",        "Mon",    5,       "Emergency department shift",
  "C10",      "avoid_pattern",  "N_MWFS", 10,      "Prefers the shorter night block"
)

requests <- staff |>
  select(doctor_id) |>
  slice_sample(n = 10) |>
  mutate(date = rota_start + sample(0:(7 * rota_weeks - 1), n()),
         request = sample(request_types, n(), replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
         weight = sample(c(2, 5), n(), replace = TRUE), notes = NA_character_) |>
  arrange(date)

# Workbook ----------------------------------------------------------------

fixed_time <- as.POSIXct("2026-11-02 12:00:00", tz = "UTC")

write_inputs_workbook(
  inputs_path,
  list(settings = settings, rules = rules, shifts = shifts, cover = cover, bank_holidays = bank_holidays,
       staff = staff, leave = leave, requests = requests, patterns = patterns, preferences = preferences,
       sequences = sequences, pattern_sequences = pattern_sequences, block_rest = block_rest),
  title = "ICU consultants (example)",
  description = paste("An example rota for 15 fictional ICU consultants covering 1st on, 2nd on and nights",
                      "for a year from 1 February 2027 (rota_type consultant: only the rules listed apply)."),
  created = fixed_time
)
message("Wrote ", inputs_path)
