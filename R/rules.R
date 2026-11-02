# Rota types and the rule registry.
#
# A rota type (the `rota_type` setting) says which sheets an inputs workbook
# must have, and whether rules left out of its `rules` sheet are on. The
# registry lists every rule once, with the code that builds it into the
# model and the code that checks a rota against it, so build_rota_model()
# and check_rota() work from the same list. A new kind of rota is a new type,
# a template workbook and registry entries for any rules it needs.

rota_types <- list(
  # Resident doctors on the 2016 TCS. Every rule is on unless switched off.
  resident = list(
    required = c("settings", "rules", "grades", "shifts", "cover", "bank_holidays", "staff",
                 "placements", "placement_roles", "leave", "requests", "skill_mix"),
    unlisted_rules_on = TRUE,
    # Phase 2 of solve_rota() frees this many days, or doctors, at a time
    search = list(days = 7, doctors = 10, whole_weeks = FALSE, first_by_week = FALSE)
  ),
  # Consultant on-call rotas: only the rules listed in the rules sheet apply
  consultant = list(
    required = c("settings", "rules", "shifts", "cover", "bank_holidays", "staff", "leave"),
    unlisted_rules_on = FALSE,
    # Rotas a year long, with blocks that should not be split. The first rota
    # is built a week at a time with every penalty counted, so it starts in
    # blocks.
    search = list(days = 28, doctors = 5, whole_weeks = TRUE, first_by_week = TRUE)
  )
)

# Sheets a rota type may leave out, as empty tables with their columns
optional_sheets <- list(
  grades = tibble::tibble(grade = character(), rank = numeric()),
  placements = tibble::tibble(doctor_id = character(), placement = character(),
                              start_date = as.Date(character()), end_date = as.Date(character())),
  placement_roles = tibble::tibble(placement = character(), role = character(), penalty = numeric()),
  requests = tibble::tibble(doctor_id = character(), date = as.Date(character()),
                            request = character(), weight = numeric()),
  skill_mix = tibble::tibble(rule_id = character(), description = character(), shifts = character(),
                             min_grade = character(), min_count = numeric(), weight = numeric()),
  patterns = tibble::tibble(pattern_id = character(), slot = character(), days = character(),
                            open_to = character()),
  preferences = tibble::tibble(doctor_id = character(), preference = character(),
                               value = character(), weight = numeric()),
  sequences = tibble::tibble(from_shift = character(), to_shift = character(),
                             days_after = numeric(), weight = numeric())
)

# Columns a sheet may leave out, and their values when it does
optional_columns <- list(
  staff = list(grade = NA_character_, pool = NA_character_, contracted_hours = NA_real_,
               ltft_fixed_day_off = NA_character_, nights_exempt = FALSE, opted_out_wtr = FALSE),
  shifts = list(is_long = FALSE, is_late = FALSE, covers = NA_character_, open_to = "all",
                label = NA_character_)
)

rota_type <- function(settings) {
  setting <- set_names(settings$value, settings$key)
  type <- if ("rota_type" %in% names(setting) && !is.na(setting[["rota_type"]])) {
    tolower(setting[["rota_type"]])
  } else {
    "resident"
  }
  if (!type %in% names(rota_types)) {
    stop("Unknown rota_type \"", type, "\" in the settings sheet. Use one of: ",
         paste(names(rota_types), collapse = ", "), call. = FALSE)
  }
  type
}

add_optional_inputs <- function(inputs) {
  for (sheet in setdiff(names(optional_sheets), names(inputs))) {
    inputs[[sheet]] <- optional_sheets[[sheet]]
  }
  for (sheet in names(optional_columns)) {
    defaults <- optional_columns[[sheet]]
    for (col in setdiff(names(defaults), names(inputs[[sheet]]))) {
      inputs[[sheet]][[col]] <- rep(defaults[[col]], nrow(inputs[[sheet]]))
    }
  }
  inputs
}

# A rule listed in the rules sheet is on unless switched off there; one left
# out is on only for rota types whose unlisted rules are on
rule_on <- function(inputs, id) {
  listed <- match(id, inputs$rules$rule_id)
  if (is.na(listed)) {
    return(rota_types[[rota_type(inputs$settings)]]$unlisted_rules_on)
  }
  !isFALSE(inputs$rules$enabled[listed])
}

# Registry -----------------------------------------------------------------

# A registry entry: the rule ids it implements (none for structural parts
# that always apply), a `model` function taking the model context and
# returning a model part (see assemble_model()), and a `check` function
# taking shift instances and inputs and returning violations. Either may be
# NULL. An entry is used when any of its rules is on; the functions handle
# rules within an entry that are off.
rule_entry <- function(rules = character(), model = NULL, check = NULL) {
  list(rules = rules, model = model, check = check)
}

rule_registry <- function() {
  list(
    rule_entry("COVER", \(ctx) cover_part(ctx$x, ctx$inputs), check_cover),
    rule_entry(model = \(ctx) list(terms = one_shift_terms(ctx$x))),
    rule_entry("MIN_REST", \(ctx) list(terms = min_rest_terms(ctx$x, ctx$shifts,
                                                              rule_value(ctx$inputs, "MIN_REST"))),
               check_min_rest),
    rule_entry("MAX_7DAY", \(ctx) list(terms = max_7day_terms(ctx$x, ctx$shifts,
                                                              rule_value(ctx$inputs, "MAX_7DAY"),
                                                              ctx$n_days)),
               check_max_7day),
    rule_entry("AVG_HOURS", \(ctx) list(terms = avg_hours_terms(ctx$x, ctx$inputs)), check_avg_hours),
    rule_entry(c("MAX_CONSEC_SHIFTS", "MAX_CONSEC_LONG", "MAX_CONSEC_LATE", "REST_AFTER_RUN", "LONG_RUN"),
               \(ctx) list(terms = run_terms(ctx$x, run_rule_table(ctx$inputs, ctx$shifts), ctx$shifts,
                                             ctx$n_days)),
               check_consecutive),
    rule_entry(c("MAX_CONSEC_NIGHTS", "REST_AFTER_NIGHTS", "SINGLE_NIGHT", "LONG_NIGHT_RUN"),
               \(ctx) list(terms = night_terms(ctx$x, ctx$shifts, ctx$inputs, ctx$n_days)),
               check_nights),
    # Weekend indicators always apply; WEEKEND_MAX is added when it is on
    rule_entry(model = \(ctx) list(terms = weekend_terms(ctx$x, ctx$weekend, ctx$touches, ctx$inputs))),
    rule_entry(c("WEEKEND_MAX", "WEEKEND_TARGET"), check = check_weekends),

    # Who may work what: applied in shift_vars() by leaving variables out
    rule_entry("IN_POST", check = check_in_post),
    rule_entry("COMPETENCY", check = check_competency),
    rule_entry(c("PLACEMENT", "PLACEMENT_AVOID"), check = check_placement),
    rule_entry("NIGHTS_EXEMPT", check = check_nights_exempt),
    rule_entry("PREFERENCE_ONLY", check = check_preference_only),
    rule_entry("LTFT_FIXED_DAY", check = check_ltft_day),
    rule_entry(c("LEAVE_APPROVED", "LEAVE_REQUESTED"), check = check_leave),
    rule_entry("MAX_SHIFT", check = check_max_shift),

    # Soft rules (R/soft.R)
    rule_entry("PLACEMENT_AVOID", \(ctx) placement_avoid_part(ctx$x, ctx$inputs)),
    rule_entry("LEAVE_REQUESTED", \(ctx) leave_requested_part(ctx$x, ctx$inputs)),
    rule_entry("REQUESTS", \(ctx) requests_part(ctx$x, ctx$weekend, ctx$inputs), check_requests),
    rule_entry("SKILL_MIX", \(ctx) skill_mix_part(ctx$x, ctx$inputs), check_skill_mix),
    rule_entry("WEEKEND_TARGET", \(ctx) weekend_target_part(ctx$weekend, ctx$inputs)),
    rule_entry("SINGLE_NIGHT", \(ctx) single_night_part(ctx$x, ctx$inputs, ctx$n_days)),
    rule_entry("LONG_RUN", \(ctx) run_target_part(ctx$x, ctx$inputs, ctx$n_days, "LONG_RUN", TRUE)),
    rule_entry("LONG_NIGHT_RUN", \(ctx) run_target_part(ctx$x, ctx$inputs, ctx$n_days, "LONG_NIGHT_RUN",
                                                        ctx$x$is_night)),
    rule_entry("ICU_SHORT_RUN", \(ctx) short_run_part(ctx$x, ctx$inputs, ctx$n_days, "ICU_SHORT_RUN",
                                                      is_icu_day(ctx$x))),
    rule_entry("ISOLATED_DAY_OFF", \(ctx) isolated_day_off_part(ctx$x, ctx$inputs, ctx$n_days)),
    rule_entry("SPLIT_WEEKEND", \(ctx) split_weekend_part(ctx$x, ctx$inputs)),
    rule_entry(c("ICU_SHORT_RUN", "ISOLATED_DAY_OFF", "SPLIT_WEEKEND"), check = check_wellbeing),
    rule_entry(c("SEQUENCE", "SEQUENCE_SOFT"), \(ctx) sequence_part(ctx$x, ctx$inputs), check_sequences),
    rule_entry("SEQUENCE_SOFT", \(ctx) sequence_soft_part(ctx$x, ctx$inputs)),
    rule_entry("PATTERNS", \(ctx) patterns_part(ctx$x, ctx$inputs), check_patterns),
    rule_entry("PATTERN_UNPREFERRED", \(ctx) unpreferred_part(ctx$x, ctx$inputs), check_unpreferred),
    rule_entry("PREFERENCES", \(ctx) preferences_part(ctx$x, ctx$inputs), check_preferences),
    rule_entry("FAIR_ONCALL", \(ctx) fair_part(filter(ctx$x, is_on_call), ctx$inputs, "FAIR_ONCALL")),
    rule_entry("FAIR_NIGHTS", \(ctx) fair_part(filter(ctx$x, is_night), ctx$inputs, "FAIR_NIGHTS")),
    rule_entry("FAIR_WEEKENDS", \(ctx) fair_part(ctx$weekend, ctx$inputs, "FAIR_WEEKENDS")),
    rule_entry("FAIR_SLOTS", \(ctx) fair_part(slot_items(ctx$x, ctx$inputs), ctx$inputs, "FAIR_SLOTS")),
    rule_entry("HOURS_TARGET", \(ctx) hours_target_part(ctx$x, ctx$inputs), check_hours_target),
    rule_entry("NORMAL_DAY_SPREAD", \(ctx) normal_day_spread_part(ctx$x, ctx$inputs),
               check_normal_day_spread)
  )
}

# Registry entries in use for these inputs
active_rules <- function(inputs) {
  keep(rule_registry(), \(entry) length(entry$rules) == 0 ||
         any(map_lgl(entry$rules, \(id) rule_on(inputs, id))))
}

# Shift variables with one row per on-call cover slot they fill, grouped by
# slot, for FAIR_SLOTS
slot_items <- function(x, inputs) {
  x |>
    inner_join(shift_covers(inputs$shifts), by = "shift_code", relationship = "many-to-many") |>
    filter(slot %in% on_call_slots(inputs)) |>
    mutate(group = slot)
}
