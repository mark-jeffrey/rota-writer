# Generate test input and example output workbooks for rota-writer.
#
#   Rscript data-raw/make_test_data.R
#
# Writes inst/extdata/rota_inputs.xlsx and inst/extdata/example_rota_output.xlsx.
# Deterministic: fixed seed and fixed workbook timestamps.

source("data-raw/template_parts.R")

set.seed(20270201)
out_dir <- "inst/extdata"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

rota_start <- ymd("2027-02-01") # Monday
rota_weeks <- 13
rota_end <- rota_start + weeks(rota_weeks) - days(1)
rota_dates <- seq(rota_start, rota_end, by = "day")

yn <- function(x) if_else(x, "Y", "N")

# Settings ----------------------------------------------------------------

settings <- tribble(
  ~key,                   ~value,                    ~notes,
  "rota_start",           format(rota_start),        "First day of rota (Monday)",
  "rota_end",             format(rota_end),          "Last day of rota (Sunday)",
  "reference_weeks",      as.character(rota_weeks),  "Reference period for average hours",
  "department",           "Anaesthesia & ICU (test)", "",
  "solver",               "highs",                   "highs or glpk",
  "solver_time_limit_s",  "600",                     "Stop and return best rota found after this"
)

# Rules -------------------------------------------------------------------
# Contract rules: 2016 TCS Schedule 3 (2019 refresh). See docs/contract-rules.md.

# Every rule in rule_library except those written for consultant rotas
rules <- rule_rows(setdiff(rule_library$rule_id, c("SEQUENCE", "SEQUENCE_SOFT", "PREFERENCE_ONLY", "PATTERNS",
                                                   "PATTERN_UNPREFERRED", "FAIR_SLOTS", "PREFERENCES", "BLOCK_REST",
                                                   "CONSEC_NIGHT_WEEKS", "PATTERN_SEQUENCE")))

# Grades ------------------------------------------------------------------

grades <- tribble(
  ~grade,      ~rank, ~description,
  "FY1",       1,     "Foundation year 1",
  "FY2",       2,     "Foundation year 2",
  "CT1",       3,     "Core trainee year 1",
  "ACCS-CT1",  3,     "Acute care common stem (emergency medicine) core trainee year 1",
  "IMT1",      3,     "Internal medicine trainee year 1",
  "CT2",       4,     "Core trainee year 2",
  "IMT2",      4,     "Internal medicine trainee year 2",
  "LED-J",     4,     "Locally employed doctor (junior clinical fellow), CT2 equivalent",
  "CT3",       5,     "Core trainee year 3",
  "IMT3",      5,     "Internal medicine trainee year 3",
  "ST3",       6,     "Specialty trainee year 3",
  "ST4",       7,     "Specialty trainee year 4",
  "LED-S",     7,     "Locally employed doctor (senior clinical fellow), ST4 equivalent",
  "ST5",       8,     "Specialty trainee year 5",
  "ST6",       9,     "Specialty trainee year 6",
  "ST7",       10,    "Specialty trainee year 7",
  "ST8",       11,    "Specialty trainee year 8"
)

# Shifts and cover --------------------------------------------------------

shifts <- tribble(
  ~shift_code, ~role,     ~shift_type,  ~start,  ~end,    ~hours,
  "ICU_DAY",   "ICU_DAY", "normal_day", "08:00", "17:00", 9,
  "ANA_DAY",   "ANA_DAY", "normal_day", "08:00", "17:00", 9,
  "ICU1_LD",   "ICU1",    "long_day",   "08:00", "20:30", 12.5,
  "ICU1_N",    "ICU1",    "night",      "20:00", "08:30", 12.5,
  "ICU2_LD",   "ICU2",    "long_day",   "08:00", "20:30", 12.5,
  "ICU2_N",    "ICU2",    "night",      "20:00", "08:30", 12.5,
  "ANA1_LD",   "ANA1",    "long_day",   "08:00", "20:30", 12.5,
  "ANA1_N",    "ANA1",    "night",      "20:00", "08:30", 12.5,
  "ANA2_LD",   "ANA2",    "long_day",   "08:00", "20:30", 12.5,
  "ANA2_N",    "ANA2",    "night",      "20:00", "08:30", 12.5,
  "OBS_LD",    "OBS",     "long_day",   "08:00", "20:30", 12.5,
  "OBS_N",     "OBS",     "night",      "20:00", "08:30", 12.5
) |>
  mutate(
    is_on_call = yn(shift_type != "normal_day"),
    is_night   = yn(shift_type == "night"),
    is_long    = yn(hours > 10),
    is_late    = "N" # no shift starts before 16:00 and finishes after 23:00
  )

# Every on-call tier runs a long day and a night every day. Normal days have
# a minimum and no maximum (blank n_max): the solver adds normal days to
# bring doctors up to their contracted hours. ICU also needs one short day
# at weekends and on bank holidays, on top of ICU1 and ICU2.
cover <- bind_rows(
  shifts |>
    filter(is_on_call == "Y") |>
    select(shift_code) |>
    crossing(day_type = c("weekday", "weekend", "bank_holiday")) |>
    mutate(n_min = 1, n_max = 1),
  tribble(
    ~shift_code, ~day_type,      ~n_min, ~n_max,
    "ICU_DAY",   "weekday",      2,      NA,
    "ANA_DAY",   "weekday",      5,      NA,
    "ICU_DAY",   "weekend",      1,      1,
    "ANA_DAY",   "weekend",      0,      0,
    "ICU_DAY",   "bank_holiday", 1,      1,
    "ANA_DAY",   "bank_holiday", 0,      0
  )
) |>
  arrange(match(shift_code, shifts$shift_code), day_type)

bank_holidays <- england_bank_holidays(rota_start, rota_end)

# Staff -------------------------------------------------------------------
# Two staffing pools, each with its own fair shares. The anaesthetics team
# covers theatres, obstetrics and ICU1 (which needs airway competence). A
# second pool of foundation, internal medicine and ACCS emergency medicine
# trainees covers ICU2. Both pools work ICU short days.
#
# Anaesthetics competencies follow grade: some CT1s lack obstetric sign-off,
# ICU1 needs airway competence, LEDs vary. ANA1 is the theatre on-call, open
# to every anaesthetist bar the ICU-only fellows. ANA2 is the senior tier,
# supporting theatres, ICU and obstetrics, so it needs ICU1 and OBS sign-off
# (though not necessarily the most senior grade on shift). The anaesthetists
# are still signed off for ICU2, but placement_roles keeps them off it. All
# names are fictional.

first_names <- c(
  "Ada", "Ben", "Cara", "Dev", "Eli", "Fern", "Gus", "Hana", "Ivo", "Jade",
  "Kit", "Lena", "Max", "Nia", "Omar", "Pia", "Quin", "Rosa", "Sam", "Tara",
  "Umar", "Vera", "Wes", "Xena", "Yusuf", "Zoe", "Arlo", "Bea", "Cal", "Dina",
  "Ezra", "Faye", "Gil", "Hope", "Ines", "Jon", "Kai", "Lola", "Milo", "Nell",
  "Otto", "Pru"
)
surnames <- c(
  "Alder", "Birch", "Cedar", "Damson", "Elm", "Fir", "Gorse", "Hazel", "Ivy",
  "Juniper", "Kale", "Larch", "Maple", "Nettle", "Oak", "Pine", "Quince",
  "Rowan", "Spruce", "Teak", "Upas", "Vetch", "Willow", "Xylo", "Yew", "Zelkova",
  "Aspen", "Beech", "Cherry", "Dogwood", "Ebony", "Fig", "Ginkgo", "Holly",
  "Ironwood", "Jacaranda", "Kauri", "Linden", "Mulberry", "Nutmeg", "Olive", "Poplar"
)

anaesthetics <- tribble(
  ~grade,  ~wte, ~ltft_fixed_day_off, ~nights_exempt, ~opted_out_wtr, ~ICU1, ~ICU2, ~ANA1, ~ANA2, ~OBS,
  # Higher trainees
  "ST8",   1.0,  NA,     FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "ST7",   1.0,  NA,     FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "ST7",   0.8,  "Fri",  FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "ST6",   1.0,  NA,     FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "ST6",   0.6,  "Wed",  FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "ST5",   1.0,  NA,     FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "ST5",   1.0,  NA,     FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "ST5",   0.8,  "Mon",  FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "ST4",   1.0,  NA,     FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "ST4",   1.0,  NA,     FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "ST4",   1.0,  NA,     TRUE,  FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  # nights exempt (occupational health)
  "ST3",   1.0,  NA,     FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "ST3",   1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,
  "ST3",   0.8,  "Thu",  FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "ST3",   1.0,  NA,     FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  # Core trainees
  "CT3",   1.0,  NA,     FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "CT3",   1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,
  "CT3",   1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,
  "CT2",   1.0,  NA,     FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "CT2",   1.0,  NA,     FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "CT2",   1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,
  "CT2",   0.8,  "Tue",  FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,
  "CT2",   1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,
  "CT1",   1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE, # no obstetric sign-off yet
  "CT1",   1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE, # no obstetric sign-off yet
  "CT1",   1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,
  # Locally employed doctors
  "LED-S", 1.0,  NA,     FALSE, TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, # ICU-only fellow
  "LED-S", 1.0,  NA,     FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "LED-J", 1.0,  NA,     FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE, FALSE, # ICU-only fellow
  "LED-J", 1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,
  "LED-J", 1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE,
  "LED-J", 0.6,  "Fri",  FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE,
  # Changeover: one leaves mid-rota, one starts mid-rota
  "ST4",   1.0,  NA,     FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  "ST3",   1.0,  NA,     FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE
)

icu_tier2 <- tribble(
  ~grade,      ~wte, ~ltft_fixed_day_off, ~nights_exempt, ~opted_out_wtr, ~ICU1, ~ICU2, ~ANA1, ~ANA2, ~OBS,
  "FY2",       1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE,
  "FY2",       1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE,
  "IMT1",      1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE,
  "IMT1",      1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE,
  "IMT2",      1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE,
  "IMT2",      0.8,  "Wed",  FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE,
  "ACCS-CT1",  1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE,
  "ACCS-CT1",  1.0,  NA,     FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE
)

staff <- bind_rows(anaesthetics = anaesthetics, icu_tier2 = icu_tier2, .id = "pool") |>
  mutate(
    doctor_id  = sprintf("D%02d", row_number()),
    name       = paste("Dr", first_names, surnames),
    start_date = rota_start,
    end_date   = rota_end,
    start_date = if_else(doctor_id == "D34", ymd("2027-03-03"), start_date),
    end_date   = if_else(doctor_id == "D33", ymd("2027-03-02"), end_date),
    across(c(nights_exempt, opted_out_wtr, ICU1:OBS), yn)
  ) |>
  rename_with(\(x) paste0("can_", x), ICU1:OBS) |>
  mutate(contracted_hours = 45 * wte) |>
  select(doctor_id, name, grade, pool, wte, contracted_hours, ltft_fixed_day_off, start_date, end_date,
         nights_exempt, opted_out_wtr, starts_with("can_"))

# Placements --------------------------------------------------------------
# 13-week rota = one 3-month block. Some doctors swap ICU <-> GENERAL halfway
# (6-week split blocks); D33/D34 hand over an ICU post at changeover. The
# ICU tier 2 pool (D35-D42) is on ICU_TIER2 throughout.

halfway <- rota_start + weeks(6)

placement_type <- c(
  D01 = "ICU", D02 = "GENERAL", D03 = "GENERAL", D04 = "ICU", D05 = "GENERAL",
  D06 = "GENERAL", D07 = "OBS", D08 = "GENERAL", D09 = "ICU", D10 = "OBS",
  D11 = "GENERAL", D12 = "ICU", D13 = "GENERAL", D14 = "OBS", D15 = "SPLIT_ICU_FIRST",
  D16 = "ICU", D17 = "GENERAL", D18 = "OBS", D19 = "ICU", D20 = "SPLIT_GEN_FIRST",
  D21 = "OBS", D22 = "OBS", D23 = "GENERAL", D24 = "GENERAL", D25 = "GENERAL",
  D26 = "GENERAL", D27 = "ICU", D28 = "GENERAL", D29 = "ICU", D30 = "OBS",
  D31 = "GENERAL", D32 = "GENERAL", D33 = "ICU", D34 = "ICU",
  set_names(rep("ICU_TIER2", nrow(icu_tier2)), sprintf("D%02d", 34 + seq_len(nrow(icu_tier2))))
)

placements <- staff |>
  select(doctor_id, start_date, end_date) |>
  mutate(type = placement_type[doctor_id]) |>
  mutate(blocks = pmap(list(type, start_date, end_date), \(type, s, e) {
    switch(type,
      SPLIT_ICU_FIRST = tibble(placement = c("ICU", "GENERAL"),
                               start_date = c(s, halfway), end_date = c(halfway - 1, e)),
      SPLIT_GEN_FIRST = tibble(placement = c("GENERAL", "ICU"),
                               start_date = c(s, halfway), end_date = c(halfway - 1, e)),
      tibble(placement = type, start_date = s, end_date = e)
    )
  })) |>
  select(doctor_id, blocks) |>
  unnest(blocks)

# Roles each placement may work. Unlisted roles are forbidden (hard).
# penalty > 0 makes a role allowed but discouraged (soft, per shift), e.g.
# general-block doctors only cover obstetrics when the OBS block can't.
placement_roles <- tribble(
  ~placement,  ~role,     ~penalty,
  "ICU",       "ICU1",    0,
  "ICU",       "ICU_DAY", 0,
  "ICU_TIER2", "ICU2",    0,
  "ICU_TIER2", "ICU_DAY", 0,
  "OBS",      "OBS",     0,
  "OBS",      "ANA_DAY", 0,
  "GENERAL",  "ANA1",    0,
  "GENERAL",  "ANA2",    0,
  "GENERAL",  "OBS",     100,
  "GENERAL",  "ANA_DAY", 0
)

# Leave -------------------------------------------------------------------

# Random leave and requests are drawn for the anaesthetics pool only, so
# adding the ICU tier 2 pool left them unchanged; its leave is listed below.
random_leave <- staff |>
  filter(pool == "anaesthetics", !doctor_id %in% c("D33", "D34")) |>
  slice_sample(n = 26) |>
  mutate(
    week       = sample(1:12, n(), replace = TRUE),
    start_date = rota_start + weeks(week - 1) + days(sample(0:2, n(), replace = TRUE)),
    end_date   = start_date + days(sample(c(2, 4, 6), n(), replace = TRUE)),
    leave_type = "annual",
    status     = sample(c("approved", "requested"), n(), replace = TRUE, prob = c(0.7, 0.3))
  ) |>
  select(doctor_id, start_date, end_date, leave_type, status)

planted_leave <- tribble(
  ~doctor_id, ~start_date,        ~end_date,          ~leave_type, ~status,     ~notes,
  # Easter clash: seven doctors want the bank holiday weekend
  "D02",      ymd("2027-03-26"),  ymd("2027-03-29"),  "annual",    "approved",  "Easter",
  "D06",      ymd("2027-03-26"),  ymd("2027-03-29"),  "annual",    "approved",  "Easter",
  "D10",      ymd("2027-03-25"),  ymd("2027-04-02"),  "annual",    "approved",  "Easter",
  "D17",      ymd("2027-03-26"),  ymd("2027-03-29"),  "annual",    "requested", "Easter",
  "D21",      ymd("2027-03-26"),  ymd("2027-03-29"),  "annual",    "requested", "Easter",
  "D24",      ymd("2027-03-22"),  ymd("2027-03-29"),  "annual",    "requested", "Easter",
  "D30",      ymd("2027-03-26"),  ymd("2027-03-29"),  "annual",    "requested", "Easter",
  # Study and exam leave
  "D16",      ymd("2027-03-10"),  ymd("2027-03-10"),  "exam",      "approved",  "Primary FRCA OSCE/SOE",
  "D17",      ymd("2027-03-10"),  ymd("2027-03-10"),  "exam",      "approved",  "Primary FRCA OSCE/SOE",
  "D13",      ymd("2027-04-14"),  ymd("2027-04-14"),  "exam",      "approved",  "Final FRCA SOE",
  "D12",      ymd("2027-04-14"),  ymd("2027-04-14"),  "exam",      "approved",  "Final FRCA SOE",
  "D05",      ymd("2027-02-17"),  ymd("2027-02-19"),  "study",     "approved",  "Regional anaesthesia course",
  "D19",      ymd("2027-03-03"),  ymd("2027-03-03"),  "study",     "approved",  "Regional teaching day",
  "D20",      ymd("2027-03-03"),  ymd("2027-03-03"),  "study",     "approved",  "Regional teaching day",
  "D23",      ymd("2027-03-03"),  ymd("2027-03-03"),  "study",     "approved",  "Regional teaching day",
  "D04",      ymd("2027-04-21"),  ymd("2027-04-23"),  "study",     "requested", "ICM conference",
  # Parental leave starting mid-rota
  "D08",      ymd("2027-04-05"),  rota_end,           "parental",  "approved",  "Parental leave",
  # ICU tier 2 pool
  "D35",      ymd("2027-02-22"),  ymd("2027-02-26"),  "annual",    "approved",  "",
  "D37",      ymd("2027-03-26"),  ymd("2027-03-29"),  "annual",    "requested", "Easter",
  "D38",      ymd("2027-03-17"),  ymd("2027-03-17"),  "study",     "approved",  "IMT regional teaching",
  "D39",      ymd("2027-03-17"),  ymd("2027-03-17"),  "study",     "approved",  "IMT regional teaching",
  "D40",      ymd("2027-04-12"),  ymd("2027-04-16"),  "annual",    "requested", "",
  "D41",      ymd("2027-03-08"),  ymd("2027-03-12"),  "annual",    "approved",  ""
)

leave <- bind_rows(planted_leave, random_leave) |>
  mutate(notes = replace_na(notes, "")) |>
  arrange(doctor_id, start_date)

# Requests ----------------------------------------------------------------


requests <- staff |>
  filter(pool == "anaesthetics", !doctor_id %in% c("D33", "D34")) |>
  slice_sample(n = 22) |>
  mutate(
    date    = sample(rota_dates, n(), replace = TRUE),
    request = sample(request_types, n(), replace = TRUE, prob = c(0.45, 0.25, 0.15, 0.15)),
    weight  = sample(c(1, 2, 3, 5), n(), replace = TRUE),
    notes   = ""
  ) |>
  select(doctor_id, date, request, weight, notes) |>
  bind_rows(tribble(
    ~doctor_id, ~date,              ~request,     ~weight, ~notes,
    "D03",      ymd("2027-02-14"),  "no_oncall",  5,       "Childcare",
    "D11",      ymd("2027-04-10"),  "no_oncall",  5,       "Wedding",
    "D11",      ymd("2027-04-11"),  "no_oncall",  5,       "Wedding",
    "D26",      ymd("2027-02-27"),  "no_weekend", 3,       "",
    "D36",      ymd("2027-03-06"),  "no_weekend", 3,       "",
    "D42",      ymd("2027-02-19"),  "no_oncall",  5,       "Family event"
  )) |>
  mutate(date = if_else(request == "no_weekend", floor_date(date, "week", week_start = 6), date)) |>
  arrange(doctor_id, date)

# Skill mix ---------------------------------------------------------------

# One senior across the ICU1, ANA2 and obstetric tiers on every shift: ST3+
# is required (heavy weight), ST5+ preferred
skill_mix <- tribble(
  ~rule_id, ~description,                                        ~shifts,                  ~min_grade, ~min_count, ~weight,
  "SM1",    "At least one ST3+ across ICU1/ANA2/OBS at night",   "ICU1_N;ANA2_N;OBS_N",    "ST3",      1,          1000,
  "SM2",    "Ideally one ST5+ across ICU1/ANA2/OBS at night",    "ICU1_N;ANA2_N;OBS_N",    "ST5",      1,          100,
  "SM3",    "At least one ST3+ across ICU1/ANA2/OBS on long days", "ICU1_LD;ANA2_LD;OBS_LD", "ST3",    1,          1000,
  "SM4",    "Ideally one ST5+ across ICU1/ANA2/OBS on long days", "ICU1_LD;ANA2_LD;OBS_LD", "ST5",     1,          100
)

# Capacity check ----------------------------------------------------------
# Crude upper bound per role: doctor-days available to that role vs slots
# required. Ignores rest rules and sharing between roles, so passing this
# is necessary but not sufficient for feasibility.

day_type_of <- function(d) {
  case_when(
    d %in% bank_holidays$date      ~ "bank_holiday",
    wday(d, week_start = 1) >= 6   ~ "weekend",
    .default = "weekday"
  )
}

leave_days <- leave |>
  filter(status == "approved") |>
  mutate(date = map2(start_date, end_date, \(s, e) seq(s, e, by = "day"))) |>
  unnest(date) |>
  distinct(doctor_id, date)

available <- staff |>
  crossing(date = rota_dates) |>
  filter(date >= start_date, date <= end_date) |>
  filter(is.na(ltft_fixed_day_off) | wday(date, label = TRUE, locale = "C") != ltft_fixed_day_off) |>
  anti_join(leave_days, by = c("doctor_id", "date")) |>
  inner_join(placements |> select(doctor_id, placement, p_start = start_date, p_end = end_date),
             by = "doctor_id", relationship = "many-to-many") |>
  filter(date >= p_start, date <= p_end) |>
  select(doctor_id, date, placement, wte, nights_exempt, starts_with("can_"))

slots <- tibble(date = rota_dates, day_type = day_type_of(rota_dates)) |>
  inner_join(cover, by = "day_type", relationship = "many-to-many") |>
  left_join(shifts, by = "shift_code") |>
  filter(is_on_call == "Y", n_min > 0)

role_days <- function(r, max_penalty = Inf) {
  allowed <- placement_roles |>
    filter(role == .env$r, penalty <= max_penalty) |>
    pull(placement)
  available |>
    filter(placement %in% allowed, .data[[paste0("can_", r)]] == "Y")
}

capacity <- slots |>
  count(role, wt = n_min, name = "slots_required") |>
  mutate(
    doctor_days_preferred = map_int(role, \(r) nrow(role_days(r, max_penalty = 0))),
    doctor_days_available = map_int(role, \(r) nrow(role_days(r))),
    ratio_preferred = round(doctor_days_preferred / slots_required, 1),
    ratio = round(doctor_days_available / slots_required, 1)
  )

# Skill mix: senior doctor-days able to work any shift in the rule, vs days
# the rule applies. A ratio near 1 means the rule will often be broken.
role_of <- set_names(shifts$role, shifts$shift_code)
skill_capacity <- skill_mix |>
  mutate(
    days_applies = nrow(distinct(slots, date)),
    senior_doctor_days = map2_int(shifts, min_grade, \(sh, g) {
      roles <- unique(role_of[str_split_1(sh, ";")])
      min_rank <- grades$rank[grades$grade == g]
      seniors <- staff$doctor_id[grades$rank[match(staff$grade, grades$grade)] >= min_rank]
      map(roles, role_days) |>
        list_rbind() |>
        filter(doctor_id %in% seniors) |>
        distinct(doctor_id, date) |>
        nrow()
    }),
    ratio = round(senior_doctor_days / days_applies, 1)
  ) |>
  select(rule_id, min_grade, days_applies, senior_doctor_days, ratio)

message("Skill mix capacity (senior doctor-days per day the rule applies):")
print(skill_capacity)

hours_needed <- sum(slots$hours * slots$n_min)
hours_cap <- sum(staff$wte * 48 * as.numeric(staff$end_date - staff$start_date + 1) / 7)

message("Capacity check (doctor-days available per on-call slot, before rest rules):")
print(capacity)
message(sprintf("On-call hours required %.0f; contracted hour cap %.0f (%.0f%% used by on-call)",
                hours_needed, hours_cap, 100 * hours_needed / hours_cap))

# Example (non-compliant) rota --------------------------------------------
# Naive round-robin over eligible doctors, ignoring rest rules, then with
# specific contract breaches planted so the checker has known targets.

planted <- tribble(
  ~doctor_id, ~date,              ~shift_code, ~violation,
  # 5 consecutive nights
  "D06",      ymd("2027-02-08"),  "ANA1_N",    "MAX_CONSEC_NIGHTS",
  "D06",      ymd("2027-02-09"),  "ANA1_N",    "MAX_CONSEC_NIGHTS",
  "D06",      ymd("2027-02-10"),  "ANA1_N",    "MAX_CONSEC_NIGHTS",
  "D06",      ymd("2027-02-11"),  "ANA1_N",    "MAX_CONSEC_NIGHTS",
  "D06",      ymd("2027-02-12"),  "ANA1_N",    "MAX_CONSEC_NIGHTS",
  # Long day straight after a night (MIN_REST and REST_AFTER_NIGHTS)
  "D09",      ymd("2027-02-22"),  "ICU1_N",    "MIN_REST",
  "D09",      ymd("2027-02-23"),  "ICU1_LD",   "MIN_REST",
  # 6 long days in 7 days = 75 h > 72 h
  "D26",      ymd("2027-03-08"),  "OBS_LD",    "MAX_7DAY",
  "D26",      ymd("2027-03-09"),  "OBS_LD",    "MAX_7DAY",
  "D26",      ymd("2027-03-10"),  "OBS_LD",    "MAX_7DAY",
  "D26",      ymd("2027-03-12"),  "OBS_LD",    "MAX_7DAY",
  "D26",      ymd("2027-03-13"),  "OBS_LD",    "MAX_7DAY",
  "D26",      ymd("2027-03-14"),  "OBS_LD",    "MAX_7DAY",
  # Rostered during approved exam leave
  "D16",      ymd("2027-03-10"),  "ICU1_N",    "LEAVE_APPROVED",
  # CT1 without obstetric sign-off on labour ward
  "D24",      ymd("2027-04-06"),  "OBS_N",     "COMPETENCY"
)

eligible <- function(r, d, max_penalty = Inf) {
  role_days(r, max_penalty) |>
    filter(date == d) |>
    pull(doctor_id)
}

assign_slots <- slots |>
  select(date, shift_code, role, shift_type) |>
  arrange(date, match(shift_code, shifts$shift_code))

pointer <- 0
assigned <- vector("list", nrow(assign_slots))
for (i in seq_len(nrow(assign_slots))) {
  slot <- assign_slots[i, ]
  fixed <- planted |> filter(date == slot$date, shift_code == slot$shift_code)
  if (nrow(fixed) > 0) {
    assigned[[i]] <- fixed$doctor_id
    next
  }
  busy <- c(
    unlist(assigned[assign_slots$date == slot$date]),
    planted$doctor_id[planted$date == slot$date]
  )
  exempt <- if (slot$shift_type == "night") staff$doctor_id[staff$nights_exempt == "Y"]
  # Prefer unpenalised placements; fall back to discouraged ones
  pool <- setdiff(eligible(slot$role, slot$date, max_penalty = 0), c(busy, exempt))
  if (length(pool) == 0) {
    pool <- setdiff(eligible(slot$role, slot$date), c(busy, exempt))
  }
  if (length(pool) == 0) {
    assigned[[i]] <- NA_character_
    next
  }
  pointer <- pointer + 1
  assigned[[i]] <- pool[(pointer %% length(pool)) + 1]
}

oncall <- assign_slots |>
  mutate(doctor_id = unlist(assigned)) |>
  select(doctor_id, date, code = shift_code)

# Normal days: anyone available on a weekday who is not on call that day or
# coming off a night gets a day shift matching their placement.
post_night <- oncall |>
  filter(str_ends(code, "_N")) |>
  transmute(doctor_id, date = date + 1)

day_work <- available |>
  filter(day_type_of(date) == "weekday") |>
  anti_join(oncall, by = c("doctor_id", "date")) |>
  anti_join(post_night, by = c("doctor_id", "date")) |>
  transmute(doctor_id, date, code = if_else(str_starts(placement, "ICU"), "ICU_DAY", "ANA_DAY"))

leave_codes <- leave |>
  mutate(
    date = map2(start_date, end_date, \(s, e) seq(s, e, by = "day")),
    code = recode(leave_type, annual = "AL", study = "SL", exam = "EX", parental = "PL")
  ) |>
  unnest(date) |>
  filter(status == "approved") |>
  distinct(doctor_id, date, code)

rota_long <- bind_rows(
  oncall,
  day_work,
  leave_codes |> anti_join(oncall, by = c("doctor_id", "date"))
) |>
  filter(!is.na(doctor_id)) |>
  arrange(date, doctor_id)

rota_grid <- rota_long |>
  pivot_wider(names_from = doctor_id, values_from = code) |>
  right_join(tibble(date = rota_dates), by = "date") |>
  arrange(date) |>
  mutate(day = wday(date, label = TRUE, locale = "C") |> as.character(), .after = date) |>
  select(date, day, all_of(staff$doctor_id))

known_violations <- planted |>
  group_by(violation, doctor_id) |>
  summarise(first_date = min(date), last_date = max(date),
            shifts = paste(unique(shift_code), collapse = ";"), .groups = "drop") |>
  mutate(note = "Planted on purpose. The round-robin also creates many unplanned breaches; this list is not exhaustive.") |>
  arrange(first_date)

unfilled <- sum(is.na(oncall$doctor_id))
message(sprintf("Example rota: %d on-call slots, %d unfilled by round-robin", nrow(oncall), unfilled))

# Write workbooks ---------------------------------------------------------

fixed_time <- as.POSIXct("2026-10-31 12:00:00", tz = "UTC")

yn_cols <- c("nights_exempt", "opted_out_wtr", paste0("can_", c("ICU1", "ICU2", "ANA1", "ANA2", "OBS")))

inputs_wb <- wb_workbook(creator = "rota-writer", datetime_created = fixed_time) |>
  add_sheet("settings", settings) |>
  add_sheet("rules", rules, list(hard_soft = c("hard", "soft"), enabled = c("Y", "N"))) |>
  add_sheet("grades", grades) |>
  add_sheet("shifts", shifts, list(
    shift_type = c("normal_day", "long_day", "night"),
    is_on_call = c("Y", "N"), is_night = c("Y", "N"), is_long = c("Y", "N"), is_late = c("Y", "N")
  )) |>
  add_sheet("cover", cover, list(day_type = c("weekday", "weekend", "bank_holiday"))) |>
  add_sheet("bank_holidays", bank_holidays) |>
  add_sheet("staff", staff, c(
    list(grade = grades$grade, ltft_fixed_day_off = c("Mon", "Tue", "Wed", "Thu", "Fri")),
    set_names(rep(list(c("Y", "N")), length(yn_cols)), yn_cols)
  )) |>
  add_sheet("placements", placements, list(placement = c("ICU", "ICU_TIER2", "OBS", "GENERAL"))) |>
  add_sheet("placement_roles", placement_roles) |>
  add_sheet("leave", leave, list(
    leave_type = c("annual", "study", "exam", "parental"),
    status = c("approved", "requested")
  )) |>
  add_sheet("requests", requests, list(request = request_types)) |>
  add_sheet("skill_mix", skill_mix, list(min_grade = grades$grade))

wb_save(inputs_wb, file.path(out_dir, "rota_inputs.xlsx"), overwrite = TRUE)

grid_dims <- wb_dims(rows = 2:(nrow(rota_grid) + 1), cols = 3:ncol(rota_grid))

output_wb <- wb_workbook(creator = "rota-writer", datetime_created = fixed_time) |>
  wb_add_dxfs_style(name = "night", bg_fill = wb_color("FF3F3F7F"), font_color = wb_color("FFFFFFFF")) |>
  wb_add_dxfs_style(name = "long_day", bg_fill = wb_color("FFFFE08A")) |>
  wb_add_dxfs_style(name = "leave", bg_fill = wb_color("FFB7E1CD")) |>
  add_sheet("grid", rota_grid) |>
  wb_freeze_pane("grid", first_active_row = 2, first_active_col = 3) |>
  wb_add_conditional_formatting("grid", dims = grid_dims, type = "containsText", rule = "_N", style = "night") |>
  wb_add_conditional_formatting("grid", dims = grid_dims, type = "containsText", rule = "_LD", style = "long_day") |>
  wb_add_conditional_formatting("grid", dims = grid_dims, type = "containsText", rule = "AL", style = "leave") |>
  wb_add_conditional_formatting("grid", dims = grid_dims, type = "containsText", rule = "SL", style = "leave") |>
  wb_add_conditional_formatting("grid", dims = grid_dims, type = "containsText", rule = "EX", style = "leave") |>
  wb_add_conditional_formatting("grid", dims = grid_dims, type = "containsText", rule = "PL", style = "leave") |>
  add_sheet("long", rota_long) |>
  add_sheet("known_violations", known_violations)

wb_save(output_wb, file.path(out_dir, "example_rota_output.xlsx"), overwrite = TRUE)

message("Wrote ", file.path(out_dir, c("rota_inputs.xlsx", "example_rota_output.xlsx")) |> paste(collapse = ", "))
