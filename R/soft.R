# Soft rules: penalties in the objective.
#
# Each is scored exactly as check_rota() scores it (the row's weight times
# the rule's weight in the `rules` sheet), so the objective equals the
# checker's total penalty. The exception is fairness, which the checker
# reports through summarise_doctors() rather than as violations.
#
# Each function returns a model part (see assemble_model()): new helper
# variables keyed by `aux`, constraint `terms`, objective `costs` and
# constant `offset`s.

soft_parts <- function(x, weekend, inputs, n_days) {
  list(
    placement_avoid_part(x, inputs),
    leave_requested_part(x, inputs),
    requests_part(x, weekend, inputs),
    skill_mix_part(x, inputs),
    weekend_target_part(weekend, inputs),
    single_night_part(x, inputs, n_days),
    fair_part(filter(x, is_on_call), inputs, "FAIR_ONCALL"),
    fair_part(filter(x, is_night), inputs, "FAIR_NIGHTS"),
    fair_part(weekend, inputs, "FAIR_WEEKENDS")
  ) |>
    compact()
}

# Rule weight, or 0 if the rule is switched off
soft_weight <- function(inputs, id) {
  if (!rule_on(inputs, id)) {
    return(0)
  }
  coalesce(inputs$rules$weight[match(id, inputs$rules$rule_id)], 1)
}

aux_vars <- function(aux, type = "C", lower = 0, upper = 1) {
  tibble::tibble(aux, type, lower, upper)
}

placement_avoid_part <- function(x, inputs) {
  weight <- soft_weight(inputs, "PLACEMENT_AVOID")
  if (weight == 0) {
    return(NULL)
  }
  list(costs = x |>
         filter(placement_penalty > 0) |>
         transmute(rule = "PLACEMENT_AVOID", j, coef = placement_penalty * weight))
}

# Per shift touching a requested leave day, including a night finishing on
# the first morning of leave
leave_requested_part <- function(x, inputs) {
  weight <- soft_weight(inputs, "LEAVE_REQUESTED")
  if (weight == 0) {
    return(NULL)
  }
  requested <- leave_days(inputs) |>
    filter(status == "requested") |>
    distinct(doctor_id, date)

  clashes <- bind_rows(
    semi_join(x, requested, by = c("doctor_id", "date")),
    x |>
      filter(end_h > 24) |>
      mutate(next_day = date + 1) |>
      semi_join(requested, by = c("doctor_id", next_day = "date"))
  )
  list(costs = transmute(distinct(clashes, j), rule = "LEAVE_REQUESTED", j, coef = weight))
}

requests_part <- function(x, weekend, inputs) {
  weight <- soft_weight(inputs, "REQUESTS")
  if (weight == 0 || nrow(inputs$requests) == 0) {
    return(NULL)
  }
  requests <- inputs$requests |>
    transmute(doctor_id, date, request, penalty = coalesce(weight, 1) * .env$weight)
  on_date <- function(type, keep) {
    requests |>
      filter(request == type) |>
      inner_join(filter(x, {{ keep }}), by = c("doctor_id", "date"), relationship = "many-to-many")
  }

  # prefer_oncall costs its weight unless an on-call is worked that day
  prefer <- filter(requests, request == "prefer_oncall")
  list(
    costs = bind_rows(
      on_date("no_oncall", is_on_call) |> transmute(j, coef = penalty),
      on_date("no_night", is_night) |> transmute(j, coef = penalty),
      on_date("prefer_oncall", is_on_call) |> transmute(j, coef = -penalty),
      requests |>
        filter(request == "no_weekend") |>
        mutate(sat = floor_saturday(date)) |>
        inner_join(weekend, by = c("doctor_id", "sat")) |>
        transmute(j, coef = penalty)
    ) |>
      mutate(rule = "REQUESTS"),
    offset = tibble::tibble(rule = "REQUESTS", value = sum(prefer$penalty))
  )
}

# short[rule, date] = 1 if fewer than min_count doctors of min_grade or above
# work the rule's shifts that day: sum(senior x) + min_count * short >= min_count
skill_mix_part <- function(x, inputs) {
  weight <- soft_weight(inputs, "SKILL_MIX")
  if (weight == 0 || nrow(inputs$skill_mix) == 0) {
    return(NULL)
  }
  rank_of <- set_names(inputs$grades$rank, inputs$grades$grade)

  sm <- inputs$skill_mix |>
    transmute(sm_id = rule_id, min_rank = rank_of[min_grade], min_count,
              penalty = coalesce(weight, 1) * .env$weight,
              shift_code = stringr::str_split(shifts, ";")) |>
    unnest(shift_code)

  # As in the checker: days when any of the rule's shifts is required
  short <- expected_cover(inputs) |>
    filter(n_min > 0) |>
    inner_join(sm, by = "shift_code", relationship = "many-to-many") |>
    distinct(sm_id, date, min_count, penalty) |>
    mutate(aux = paste("SKILL_MIX", sm_id, date))
  seniors <- x |>
    mutate(rank = rank_of[inputs$staff$grade[match(doctor_id, inputs$staff$doctor_id)]]) |>
    inner_join(sm, by = "shift_code", relationship = "many-to-many") |>
    filter(rank >= min_rank) |>
    inner_join(select(short, sm_id, date, aux), by = c("sm_id", "date"))

  list(
    vars = aux_vars(short$aux, type = if_else(short$min_count > 1, "I", "C")),
    terms = bind_rows(
      transmute(seniors, con = aux, j, coef = 1, lo = min_count),
      transmute(short, con = aux, aux, coef = min_count, lo = min_count)
    ) |>
      mutate(rule = "SKILL_MIX", hi = Inf),
    costs = transmute(short, rule = "SKILL_MIX", aux, coef = penalty)
  )
}

# excess[d] >= counted weekends worked - floor(counted weekends / N)
weekend_target_part <- function(weekend, inputs) {
  weight <- soft_weight(inputs, "WEEKEND_TARGET")
  if (weight == 0) {
    return(NULL)
  }
  one_in <- rule_value(inputs, "WEEKEND_TARGET")
  allowed <- counted_weekends(inputs) |>
    count(doctor_id, name = "total") |>
    transmute(doctor_id, allowed = floor(total / one_in), aux = paste("WEEKEND_TARGET", doctor_id))
  worked <- weekend |>
    filter(counted) |>
    inner_join(allowed, by = "doctor_id")

  list(
    vars = aux_vars(allowed$aux, upper = Inf),
    terms = bind_rows(
      transmute(worked, con = aux, j, coef = 1, hi = allowed),
      transmute(allowed, con = aux, aux, coef = -1, hi = allowed)
    ) |>
      mutate(rule = "WEEKEND_TARGET", lo = -Inf),
    costs = transmute(allowed, rule = "WEEKEND_TARGET", aux, coef = weight)
  )
}

# single[d, t] >= night[t] - night[t - 1] - night[t + 1]
single_night_part <- function(x, inputs, n_days) {
  weight <- soft_weight(inputs, "SINGLE_NIGHT")
  nights <- filter(x, is_night)
  if (weight == 0 || nrow(nights) == 0) {
    return(NULL)
  }
  singles <- distinct(nights, doctor_id, t) |>
    mutate(aux = paste("SINGLE_NIGHT", doctor_id, t))
  neighbours <- singles |>
    cross_join(tibble::tibble(dt = c(-1L, 1L))) |>
    mutate(t = t + dt) |>
    inner_join(select(nights, doctor_id, t, j), by = c("doctor_id", "t"))

  list(
    vars = aux_vars(singles$aux),
    terms = bind_rows(
      nights |> inner_join(select(singles, doctor_id, t, con = aux), by = c("doctor_id", "t")) |>
        transmute(con, j, coef = 1),
      transmute(neighbours, con = aux, j, coef = -1),
      transmute(singles, con = aux, aux, coef = -1)
    ) |>
      mutate(rule = "SINGLE_NIGHT", lo = -Inf, hi = 0),
    costs = transmute(singles, rule = "SINGLE_NIGHT", aux, coef = weight)
  )
}

# Distance of each doctor's count from their WTE-weighted share of the total,
# as in summarise_doctors(), but shared only among doctors who can work the
# category at all (e.g. not nights-exempt doctors for nights). The penalty
# is piecewise linear and convex, so the k-th unit away from the share costs
# (2k - 1) x weight (approximately quadratic). This spreads the unavoidable
# imbalance across doctors instead of loading it onto one.
fair_part <- function(items, inputs, rule, steps = 3) {
  weight <- soft_weight(inputs, rule)
  if (weight == 0 || nrow(items) == 0) {
    return(NULL)
  }
  total <- paste(rule, "total")
  share <- inputs$staff |>
    filter(doctor_id %in% items$doctor_id) |>
    transmute(
      doctor_id,
      weeks = as.numeric(pmin(end_date, inputs$period$end) - pmax(start_date, inputs$period$start) + 1) / 7,
      share = wte * weeks / sum(wte * weeks),
      con = paste(rule, doctor_id)
    )
  segments <- share |>
    tidyr::crossing(dir = c(1, -1), step = seq_len(steps + 1)) |>
    mutate(aux = paste(con, dir, step),
           upper = if_else(step > steps, Inf, 1),
           cost = weight * (2 * step - 1))

  list(
    vars = bind_rows(aux_vars(total, upper = Inf), aux_vars(segments$aux, upper = segments$upper)),
    terms = bind_rows(
      # total = sum of all items
      transmute(items, con = total, j, coef = 1),
      tibble::tibble(con = total, aux = total, coef = -1),
      # count - share x total - over + under = 0
      items |> inner_join(select(share, doctor_id, con), by = "doctor_id") |> transmute(con, j, coef = 1),
      transmute(share, con, aux = total, coef = -share),
      transmute(segments, con, aux, coef = -dir)
    ) |>
      mutate(rule = rule, lo = 0, hi = 0),
    costs = transmute(segments, rule = rule, aux, coef = cost)
  )
}
