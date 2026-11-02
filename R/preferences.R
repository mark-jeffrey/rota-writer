# Standing preferences (the optional `preferences` sheet, rule PREFERENCES):
# one row per doctor and preference, with a weight. Pattern preferences
# (prefer_pattern, avoid_pattern) are costed with the blocks in
# R/patterns.R; the rest cost each shift worked against them:
# * prefer_shift S: any other shift filling a slot S fills, e.g. a 2nd-on
#   day without the night for a doctor who prefers 24-hour shifts. A shift
#   whose `open_to` is "preference" is only open to doctors who prefer it.
# * avoid_shift S: each S.
# * day_off Mon...Sun: each shift starting that weekday.

preference_types <- c("prefer_shift", "avoid_shift", "day_off", "prefer_pattern", "avoid_pattern")

# Shift-level preference costs (before the rule weight): doctor_id, then
# either shift_code or weekday, weight and what the preference is
shift_preference_costs <- function(inputs) {
  prefs <- mutate(inputs$preferences, weight = coalesce(weight, 1))
  covers <- shift_covers(inputs$shifts)
  prefer <- prefs |>
    filter(preference == "prefer_shift") |>
    select(doctor_id, preferred = value, weight) |>
    inner_join(covers, by = c(preferred = "shift_code"), relationship = "many-to-many") |>
    inner_join(rename(covers, other = shift_code), by = "slot", relationship = "many-to-many") |>
    filter(other != preferred) |>
    distinct(doctor_id, preferred, shift_code = other, weight) |>
    transmute(doctor_id, shift_code, weight, what = paste("prefers", preferred))
  avoid <- prefs |>
    filter(preference == "avoid_shift") |>
    transmute(doctor_id, shift_code = value, weight, what = paste("avoids", value))
  day_off <- prefs |>
    filter(preference == "day_off") |>
    transmute(doctor_id, weekday = value, weight, what = paste("prefers", value, "off"))
  bind_rows(prefer, avoid, day_off)
}

# Each worked shift (doctor_id, date, shift_code, ...) against the
# preferences it goes against: one row per shift and preference
shift_preference_hits <- function(worked, inputs) {
  costs <- shift_preference_costs(inputs)
  if (nrow(costs) == 0) {
    return(NULL)
  }
  if (!"weekday" %in% names(costs)) costs$weekday <- NA_character_
  if (!"shift_code" %in% names(costs)) costs$shift_code <- NA_character_
  worked <- mutate(worked, weekday = weekday_of(date))
  bind_rows(
    inner_join(worked, select(filter(costs, !is.na(shift_code)), -weekday),
               by = c("doctor_id", "shift_code"), relationship = "many-to-many"),
    inner_join(worked, select(filter(costs, !is.na(weekday)), -shift_code),
               by = c("doctor_id", "weekday"), relationship = "many-to-many")
  )
}

preferences_part <- function(x, inputs) {
  weight <- soft_weight(inputs, "PREFERENCES")
  if (weight == 0) {
    return(NULL)
  }
  hits <- shift_preference_hits(x, inputs)
  if (is.null(hits) || nrow(hits) == 0) {
    return(NULL)
  }
  list(costs = transmute(hits, rule = "PREFERENCES", j, coef = .data$weight * .env$weight))
}

check_preferences <- function(si, inputs) {
  hits <- shift_preference_hits(rename(si, shift_code = code), inputs)
  if (is.null(hits) || nrow(hits) == 0) {
    return(NULL)
  }
  with(hits, violations("PREFERENCES", doctor_id, date, weight = weight,
                        detail = paste(shift_code, "-", what)))
}
