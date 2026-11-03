# Soft rules: penalties in the objective.
#
# Each is scored exactly as check_rota() scores it (the row's weight times
# the rule's weight in the `rules` sheet), so the objective equals the
# checker's total penalty. The exception is fairness, which the checker
# reports through summarise_doctors() rather than as violations.
#
# Each function returns a model part (see assemble_model()): new helper
# variables keyed by `aux`, constraint `terms`, objective `costs` and
# constant `offset`s. The registry (R/rules.R) says when each is used.

# Rule weight, or 0 if the rule is switched off or not in the rules sheet
soft_weight <- function(inputs, id) {
  if (!rule_on(inputs, id) || !id %in% inputs$rules$rule_id) {
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

# Nights in two Monday-Sunday weeks running, such as night blocks back to
# back (per pair of weeks). With any[d, w] 1 if the doctor works a night in
# week w:
#   any[d, w] >= x[d, t, night] for each night in the week
#   pair[d, w] >= any[d, w] + any[d, w + 1] - 1
consec_night_weeks_part <- function(x, inputs) {
  weight <- soft_weight(inputs, "CONSEC_NIGHT_WEEKS")
  if (weight == 0) {
    return(NULL)
  }
  nights <- x |>
    filter(is_night) |>
    mutate(week = t - lubridate::wday(date, week_start = 1) + 1L) |>
    filter(week %in% rota_weeks(inputs))
  weeks <- distinct(nights, doctor_id, week) |>
    mutate(any = paste("CONSEC_NIGHT_WEEKS", doctor_id, week))
  pairs <- weeks |>
    inner_join(transmute(weeks, doctor_id, week = week - 7L, next_any = any), by = c("doctor_id", "week")) |>
    mutate(aux = paste(any, "and next"))
  if (nrow(pairs) == 0) {
    return(NULL)
  }
  weeks <- filter(weeks, any %in% c(pairs$any, pairs$next_any))
  nights <- inner_join(nights, weeks, by = c("doctor_id", "week")) |>
    mutate(con = paste(any, j))

  list(
    vars = aux_vars(c(weeks$any, pairs$aux)),
    terms = bind_rows(
      transmute(nights, con, j, coef = 1, hi = 0),
      transmute(nights, con, aux = any, coef = -1, hi = 0),
      transmute(pairs, con = aux, aux = any, coef = 1, hi = 1),
      transmute(pairs, con = aux, aux = next_any, coef = 1, hi = 1),
      transmute(pairs, con = aux, aux, coef = -1, hi = 1)
    ) |>
      mutate(rule = "CONSEC_NIGHT_WEEKS", lo = -Inf),
    costs = transmute(pairs, rule = "CONSEC_NIGHT_WEEKS", aux, coef = weight)
  )
}

# Runs longer than the rule's value n, of any shift (LONG_RUN) or of nights
# (LONG_NIGHT_RUN). A run of L days fills L - n windows of n + 1 days, so
# each day beyond n costs the weight:
#   excess[d, w] >= sum(x on days w ... w + n) - n
run_target_part <- function(x, inputs, n_days, rule, keep) {
  weight <- soft_weight(inputs, rule)
  n <- rule_value(inputs, rule)
  x <- x[keep, ]
  if (weight == 0 || length(n) == 0 || is.na(n) || nrow(x) == 0) {
    return(NULL)
  }
  in_window <- x |>
    select(doctor_id, t, j) |>
    cross_join(tibble::tibble(back = 0:n)) |>
    mutate(w = t - back) |>
    filter(w >= 1, w + n <= n_days) |>
    mutate(con = paste(rule, doctor_id, w))
  # Windows with a day the doctor cannot work can never be full
  windows <- in_window |>
    summarise(days = n_distinct(t), .by = con) |>
    filter(days > n)
  in_window <- semi_join(in_window, windows, by = "con")

  list(
    vars = aux_vars(windows$con),
    terms = bind_rows(
      transmute(in_window, con, j, coef = 1),
      transmute(windows, con, aux = con, coef = -1)
    ) |>
      mutate(rule = rule, lo = -Inf, hi = n),
    costs = transmute(windows, rule = rule, aux = con, coef = weight)
  )
}

# ICU day-time shifts (normal and long days), whose runs give continuity
is_icu_day <- function(shifts) {
  !shifts$is_night & stringr::str_starts(shifts$role, "ICU")
}

# Runs of L < n days (the rule's value), with a day outside the run on each
# side inside the rota, cost (n - L) x weight:
#   short[d, t, L] >= sum(x on days t ... t + L - 1) - x[t - 1] - x[t + L] - (L - 1)
short_run_part <- function(x, inputs, n_days, rule, keep) {
  weight <- soft_weight(inputs, rule)
  n <- rule_value(inputs, rule)
  x <- x[keep, ]
  if (weight == 0 || length(n) == 0 || is.na(n) || n < 2 || nrow(x) == 0) {
    return(NULL)
  }
  days <- distinct(x, doctor_id, t)
  runs <- days |>
    tidyr::crossing(len = seq_len(n - 1)) |>
    filter(t > 1, t + len <= n_days) |>
    cross_join(tibble::tibble(i = 0:(n - 2))) |>
    filter(i < len) |>
    mutate(day = t + i) |>
    semi_join(days, by = c("doctor_id", day = "t")) |>
    summarise(days = n(), .by = c(doctor_id, t, len)) |>
    filter(days == len) |>
    mutate(aux = paste(rule, doctor_id, t, len))
  cells <- runs |>
    tidyr::crossing(day = -1:(n - 1)) |>
    filter(day <= len) |>
    transmute(aux, doctor_id, t = t + day, coef = if_else(day == -1 | day == len, -1, 1))

  list(
    vars = aux_vars(runs$aux),
    terms = bind_rows(
      cells |>
        inner_join(select(x, doctor_id, t, j), by = c("doctor_id", "t"),
                   relationship = "many-to-many") |>
        transmute(con = aux, j, coef),
      transmute(runs, con = aux, aux, coef = -1)
    ) |>
      mutate(hi = runs$len[match(con, runs$aux)] - 1, rule = rule, lo = -Inf),
    costs = transmute(runs, rule = rule, aux, coef = (n - len) * weight)
  )
}

# A single day off (not leave) between two working days:
#   single[d, t] >= x[t - 1] + x[t + 1] - x[t] - 1
isolated_day_off_part <- function(x, inputs, n_days) {
  weight <- soft_weight(inputs, "ISOLATED_DAY_OFF")
  if (weight == 0 || nrow(x) == 0) {
    return(NULL)
  }
  days <- distinct(x, doctor_id, t)
  on_leave <- leave_days(inputs) |>
    transmute(doctor_id, t = as.integer(date - inputs$period$start) + 1L)
  gaps <- days |>
    mutate(t = t + 1L) |>
    filter(t < n_days) |>
    semi_join(mutate(days, t = t - 1L), by = c("doctor_id", "t")) |>
    anti_join(on_leave, by = c("doctor_id", "t")) |>
    mutate(aux = paste("ISOLATED_DAY_OFF", doctor_id, t))
  cells <- gaps |>
    tidyr::crossing(day = -1:1) |>
    transmute(aux, doctor_id, t = t + day, coef = if_else(day == 0, -1, 1))

  list(
    vars = aux_vars(gaps$aux),
    terms = bind_rows(
      cells |>
        inner_join(select(x, doctor_id, t, j), by = c("doctor_id", "t"),
                   relationship = "many-to-many") |>
        transmute(con = aux, j, coef),
      transmute(gaps, con = aux, aux, coef = -1)
    ) |>
      mutate(rule = "ISOLATED_DAY_OFF", lo = -Inf, hi = 1),
    costs = transmute(gaps, rule = "ISOLATED_DAY_OFF", aux, coef = weight)
  )
}

# Working (any time on) Saturday or Sunday but not both. For each shift a
# touching one day, with B the shifts touching the other:
#   split[d, sat] >= x[a] - sum(x[B])
split_weekend_part <- function(x, inputs) {
  weight <- soft_weight(inputs, "SPLIT_WEEKEND")
  if (weight == 0 || nrow(x) == 0) {
    return(NULL)
  }
  touch <- bind_rows(
    select(x, doctor_id, day = date, j),
    x |> filter(end_h > 24) |> transmute(doctor_id, day = date + 1, j)
  ) |>
    mutate(sat = floor_saturday(day), is_sat = day == sat) |>
    filter(day <= sat + 1, sat >= inputs$period$start, sat + 1 <= inputs$period$end) |>
    mutate(aux = paste("SPLIT_WEEKEND", doctor_id, sat))
  weekends <- distinct(touch, aux)
  a <- mutate(touch, con = paste(aux, j, is_sat))

  list(
    vars = aux_vars(weekends$aux),
    terms = bind_rows(
      transmute(a, con, j, coef = 1),
      a |>
        select(con, aux, is_sat) |>
        inner_join(select(touch, aux, other = is_sat, j), by = "aux",
                   relationship = "many-to-many") |>
        filter(other != is_sat) |>
        transmute(con, j, coef = -1),
      transmute(a, con, aux, coef = -1)
    ) |>
      # a Saturday night touches both days, so it cancels out of its own row
      summarise(coef = sum(coef), .by = c(con, j, aux)) |>
      filter(coef != 0) |>
      mutate(rule = "SPLIT_WEEKEND", lo = -Inf, hi = 0),
    costs = transmute(weekends, rule = "SPLIT_WEEKEND", aux, coef = weight)
  )
}

# Distance of each doctor's count from their WTE-weighted share of their
# pool's total, as in summarise_doctors(), but shared only among doctors who
# can work the category at all (e.g. not nights-exempt doctors for nights).
# Each pool has its own total, so one pool's work never counts towards
# another's shares. The penalty
# is piecewise linear and convex, so the k-th unit away from the share costs
# (2k - 1) x weight (approximately quadratic). This spreads the unavoidable
# imbalance across doctors instead of loading it onto one.
#
# Items may have a `group` column (e.g. the cover slot for FAIR_SLOTS) to
# share each group separately.
fair_part <- function(items, inputs, rule, steps = 3) {
  weight <- soft_weight(inputs, rule)
  if (weight == 0 || nrow(items) == 0) {
    return(NULL)
  }
  if (!"group" %in% names(items)) {
    items$group <- ""
  }
  in_group <- \(key, group) if_else(group == "", key, paste(key, group))
  share <- inputs$staff |>
    mutate(pool = staff_pool(inputs)) |>
    inner_join(distinct(items, doctor_id, group), by = "doctor_id") |>
    mutate(
      weeks = as.numeric(pmin(end_date, inputs$period$end) - pmax(start_date, inputs$period$start) + 1) / 7,
      share = wte * weeks / sum(wte * weeks),
      .by = c(pool, group)
    ) |>
    transmute(doctor_id, group, share, con = in_group(paste(rule, doctor_id), group),
              total = in_group(paste(rule, "total", pool), group))
  totals <- distinct(share, total)
  items <- inner_join(items, select(share, doctor_id, group, con, total), by = c("doctor_id", "group"))
  segments <- share |>
    tidyr::crossing(dir = c(1, -1), step = seq_len(steps + 1)) |>
    mutate(aux = paste(con, dir, step),
           upper = if_else(step > steps, Inf, 1),
           cost = weight * (2 * step - 1))

  list(
    vars = bind_rows(aux_vars(totals$total, upper = Inf), aux_vars(segments$aux, upper = segments$upper)),
    terms = bind_rows(
      # pool total = sum of the pool's items
      transmute(items, con = total, j, coef = 1),
      transmute(totals, con = total, aux = total, coef = -1),
      # count - share x pool total - over + under = 0
      transmute(items, con, j, coef = 1),
      transmute(share, con, aux = total, coef = -share),
      transmute(segments, con, aux, coef = -dir)
    ) |>
      mutate(rule = rule, lo = 0, hi = 0),
    costs = transmute(segments, rule = rule, aux, coef = cost)
  )
}

# Each doctor's hours (rostered plus approved leave, see leave_hours())
# close to contracted hours x weeks in post:
#   rostered hours + short - over = contracted total - leave hours
# Hours short cost more for each further normal day short (the k-th day's
# hours cost (2k - 1) x weight each), so a shortfall is shared between
# doctors. Hours over cost 10 x weight each, so doctors go over their
# contracted hours only when cover needs it.
hours_target_part <- function(x, inputs, steps = 3) {
  weight <- soft_weight(inputs, "HOURS_TARGET")
  if (weight == 0) {
    return(NULL)
  }
  normal_day <- inputs$shifts$hours[inputs$shifts$shift_type == "normal_day"]
  day_hours <- if (length(normal_day) > 0) max(normal_day) else 8

  target <- inputs$staff |>
    transmute(doctor_id, total = contracted_hours(inputs) * weeks_in_post(inputs)) |>
    filter(!is.na(total), doctor_id %in% x$doctor_id) |>
    left_join(leave_hours(inputs), by = "doctor_id") |>
    mutate(total = total - coalesce(leave_hours, 0), con = paste("HOURS_TARGET", doctor_id))
  short <- target |>
    tidyr::crossing(step = seq_len(steps + 1)) |>
    mutate(aux = paste(con, "short", step),
           upper = if_else(step > steps, Inf, day_hours),
           cost = weight * (2 * step - 1))
  over <- mutate(target, aux = paste(con, "over"))

  list(
    vars = bind_rows(aux_vars(short$aux, upper = short$upper), aux_vars(over$aux, upper = Inf)),
    terms = bind_rows(
      x |>
        inner_join(select(target, doctor_id, con), by = "doctor_id") |>
        transmute(con, j, coef = hours),
      transmute(short, con, aux, coef = 1),
      transmute(over, con, aux, coef = -1)
    ) |>
      left_join(select(target, con, total), by = "con") |>
      transmute(rule = "HOURS_TARGET", con, j, aux, coef, lo = total, hi = total),
    costs = bind_rows(
      transmute(short, rule = "HOURS_TARGET", aux, coef = cost),
      transmute(over, rule = "HOURS_TARGET", aux, coef = 10 * weight)
    )
  )
}

# Normal-day staffing even across the rota: each day's count of each
# normal-day shift close to a common level for that shift and day type
# (weekday, weekend, bank holiday), chosen by the solver. Each doctor-day
# above or below the level costs `weight`.
normal_day_spread_part <- function(x, inputs) {
  weight <- soft_weight(inputs, "NORMAL_DAY_SPREAD")
  normal <- inputs$shifts$shift_code[inputs$shifts$shift_type == "normal_day"]
  if (weight == 0 || length(normal) == 0) {
    return(NULL)
  }
  days <- expected_cover(inputs) |>
    filter(shift_code %in% normal, n_max > 0) |>
    transmute(date, shift_code, con = paste("NORMAL_DAY_SPREAD", shift_code, date),
              level = paste("NORMAL_DAY_SPREAD", shift_code, day_type, "level"))
  levels <- distinct(days, level)
  devs <- days |>
    tidyr::crossing(dir = c(1, -1)) |>
    mutate(aux = paste(con, dir))

  list(
    vars = bind_rows(aux_vars(levels$level, upper = Inf), aux_vars(devs$aux, upper = Inf)),
    terms = bind_rows(
      x |>
        inner_join(select(days, date, shift_code, con), by = c("date", "shift_code")) |>
        transmute(con, j, coef = 1),
      transmute(days, con, aux = level, coef = -1),
      transmute(devs, con, aux, coef = -dir)
    ) |>
      mutate(rule = "NORMAL_DAY_SPREAD", lo = 0, hi = 0),
    costs = transmute(devs, rule = "NORMAL_DAY_SPREAD", aux, coef = weight)
  )
}
