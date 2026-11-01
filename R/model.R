# The rota as a mixed-integer linear programme (MILP).
#
# Day-index formulation: binary x[d, t, s] is 1 if doctor d works shift s on
# day t (t = 1 is the rota start). Shift times are hours from 00:00 on the day
# the shift starts, so a night is 20:00-32:30. Rest rules are written in clock
# time from these hours, so the model agrees with the checker without
# hard-coding shift patterns.
#
# The model stays as tidy tables until it is solved: `vars` has one row per
# variable (column j), `terms` one row per non-zero coefficient (row i,
# column j), and `rows` the bounds lo <= row <= hi. Each hard constraint
# family below returns terms with columns rule, con (a key unique within the
# rule), j, coef, lo and hi. Soft rules (R/soft.R) also add helper variables
# and objective costs. Rules switched off in the `rules` sheet are left out.

#' Build the rota optimisation model
#'
#' Turns the inputs into a mixed-integer linear programme with one binary
#' variable per doctor, day and shift that the doctor may work.
#'
#' Hard rules are constraints: one shift per day, in post,
#' competency, placement, nights-exempt, LTFT fixed days, approved leave, and
#' the TCS Schedule 3 limits on hours, rest, consecutive shifts, nights and
#' weekends. Cover is hard too, except that a slot may be left unfilled at a
#' cost of 100,000 (or the `COVER` weight in the `rules` sheet, if set). That
#' only happens when the other hard rules leave no way to fill it, so
#' impossible inputs still give a rota, with the gaps showing which slots
#' could not be filled. Soft rules are penalties in the objective, scored as in
#' [check_rota()]: requested leave, requests, discouraged placement roles,
#' skill mix, the 1-in-3 weekend target, isolated single nights and runs of
#' shifts or nights longer than their targets, short ICU runs, single days off
#' and split weekends. Fairness
#' (`FAIR_ONCALL`, `FAIR_NIGHTS`, `FAIR_WEEKENDS`) penalises each doctor's
#' distance from their WTE-weighted share, with the cost per shift rising the
#' further they are from it, so the solver spreads work evenly.
#'
#' Most users want [solve_rota()], which calls this.
#'
#' @inheritParams check_rota
#'
#' @returns A `rota_model`: a list of tibbles.
#'   \describe{
#'     \item{vars}{One row per variable: `j`, `kind` (`"x"` for doctor/day/
#'       shift, `"weekend"` for weekend-worked indicators, `"aux"` for other
#'       helper variables), `type` (`"I"` or `"C"`), bounds and `cost`. Shift
#'       variables also have `doctor_id`, `date`, `t` and `shift_code`.}
#'     \item{rows}{One row per constraint: `i`, `rule` (matching the
#'       `rules` sheet where there is one), `con` and the bounds `lo`, `hi`.}
#'     \item{terms}{Non-zero coefficients: `i`, `j`, `coef`.}
#'     \item{costs}{Objective coefficients by soft rule: `rule`, `j`, `coef`.
#'       `vars$cost` is their sum per variable.}
#'     \item{offset}{Constant objective terms by rule (`rule`, `value`), e.g.
#'       the penalty for a `prefer_oncall` request, cancelled by working an
#'       on-call that day.}
#'   }
#'
#' @export
#' @examples
#' inputs <- read_rota_inputs(system.file("extdata", "rota_inputs.xlsx", package = "rotawriter"))
#' build_rota_model(inputs)
build_rota_model <- function(inputs) {
  inputs <- as_rota_inputs(inputs)
  shifts <- shift_times(inputs$shifts)
  n_days <- length(rota_dates(inputs))

  x <- shift_vars(inputs, shifts) |>
    mutate(j = row_number())

  run_rules <- run_rule_table(inputs, shifts)
  touches <- weekend_touches(x)
  vars <- bind_rows(
    mutate(x, kind = "x", type = "I"),
    weekend_vars(inputs, touches) |> mutate(kind = "weekend", type = "C")
  ) |>
    mutate(j = row_number(), lower = 0, upper = 1) # shift variables keep their j
  x <- filter(vars, kind == "x")
  weekend <- filter(vars, kind == "weekend")

  hard <- list(
    one_shift_terms(x),
    if (rule_on(inputs, "MIN_REST")) min_rest_terms(x, shifts, rule_value(inputs, "MIN_REST")),
    if (rule_on(inputs, "MAX_7DAY")) max_7day_terms(x, shifts, rule_value(inputs, "MAX_7DAY"), n_days),
    if (rule_on(inputs, "AVG_HOURS")) avg_hours_terms(x, inputs),
    run_terms(x, run_rules, shifts, n_days),
    night_terms(x, shifts, inputs, n_days),
    weekend_terms(x, weekend, touches, inputs)
  )
  soft <- soft_parts(x, weekend, inputs, n_days)

  assemble_model(vars, c(list(cover_part(x, inputs)), map(hard, \(terms) list(terms = terms)), soft))
}

# Combine model parts, each a list of optional tibbles: `vars` (new helper
# variables, keyed by `aux`), `terms`, `costs` and `offset`. Terms and costs
# refer to variables by `j`, or to new helper variables by `aux`.
assemble_model <- function(vars, parts) {
  pick <- \(name) list_rbind(map(parts, name))
  aux <- pick("vars")
  if (nrow(aux) > 0) {
    vars <- bind_rows(vars, mutate(aux, kind = "aux", j = nrow(vars) + row_number()))
  }
  resolve <- function(df) {
    if (!"aux" %in% names(df)) {
      return(df)
    }
    if (!"j" %in% names(df)) df$j <- NA_integer_
    df |>
      mutate(j = coalesce(j, vars$j[match(aux, vars$aux)])) |>
      select(-aux)
  }

  terms <- pick("terms") |>
    resolve() |>
    mutate(i = cur_group_id(), .by = c(rule, con))
  rows <- terms |>
    distinct(i, rule, con, lo, hi) |>
    arrange(i)
  stopifnot(!anyDuplicated(rows$i), !anyNA(terms$j))

  costs <- resolve(pick("costs"))
  costs <- if (nrow(costs) == 0) tibble::tibble(rule = character(), j = integer(), coef = numeric()) else
    summarise(costs, coef = sum(coef), .by = c(rule, j))
  offset <- pick("offset")
  offset <- if (nrow(offset) == 0) tibble::tibble(rule = character(), value = numeric()) else
    summarise(offset, value = sum(value), .by = rule)

  vars$cost <- 0
  by_var <- summarise(costs, coef = sum(coef), .by = j)
  vars$cost[by_var$j] <- by_var$coef

  structure(
    list(vars = vars, rows = rows, terms = select(terms, i, j, coef), costs = costs, offset = offset),
    class = c("rota_model", "list")
  )
}

#' @export
print.rota_model <- function(x, ...) {
  cat(sprintf("<rota_model> %d variables (%d binary), %d constraints, %d non-zeros\n",
              nrow(x$vars), sum(x$vars$type == "I"), nrow(x$rows), nrow(x$terms)))
  by_rule <- count(x$rows, rule)
  cat(sprintf("  %-18s %6d rows\n", by_rule$rule, by_rule$n), sep = "")
  invisible(x)
}

# Rules are on unless switched off in the rules sheet
rule_on <- function(inputs, id) {
  !isFALSE(inputs$rules$enabled[match(id, inputs$rules$rule_id)])
}

hhmm_hours <- function(x) {
  as.numeric(substr(x, 1, 2)) + as.numeric(substr(x, 4, 5)) / 60
}

shift_times <- function(shifts) {
  mutate(shifts, start_h = hhmm_hours(start), end_h = start_h + hours)
}

# Gap in hours from the end of s1 on day 0 to the start of s2 on day k
shift_gaps <- function(shifts, k) {
  tidyr::crossing(
    select(shifts, s1 = shift_code, end1 = end_h),
    select(shifts, s2 = shift_code, start2 = start_h),
    k = k
  ) |>
    mutate(gap = 24 * k + start2 - end1) |>
    select(s1, s2, k, gap)
}

# Variables -------------------------------------------------------------

# Every (doctor, day, shift) the doctor may work. Hard rules about who may
# work what are applied here by leaving variables out.
shift_vars <- function(inputs, shifts) {
  staff <- inputs$staff
  needed <- expected_cover(inputs) |>
    filter(n_max > 0) |>
    select(date, shift_code)

  doctor_days <- if (rule_on(inputs, "IN_POST")) {
    in_post_days(inputs)
  } else {
    tidyr::crossing(doctor_id = staff$doctor_id, date = rota_dates(inputs))
  }

  x <- doctor_days |>
    inner_join(needed, by = "date", relationship = "many-to-many") |>
    inner_join(select(shifts, shift_code, role, hours, start_h, end_h, is_on_call, is_night,
                      is_long, is_late),
               by = "shift_code")

  # Unlisted roles are forbidden; listed ones may carry a soft penalty
  allowed <- inputs$placements |>
    inner_join(inputs$placement_roles, by = "placement", relationship = "many-to-many") |>
    select(doctor_id, role, p_start = start_date, p_end = end_date, placement_penalty = penalty)
  x <- x |>
    left_join(allowed, join_by(doctor_id, role, between(date, p_start, p_end)),
              relationship = "many-to-many") |>
    select(-p_start, -p_end) |>
    arrange(placement_penalty)
  if (rule_on(inputs, "PLACEMENT")) {
    x <- filter(x, !is.na(placement_penalty))
  }
  x <- mutate(x, placement_penalty = coalesce(placement_penalty, 0))

  if (rule_on(inputs, "COMPETENCY")) {
    cannot <- staff |>
      select(doctor_id, starts_with("can_")) |>
      pivot_longer(-doctor_id, names_to = "role", names_prefix = "can_", values_to = "can") |>
      filter(!can)
    x <- anti_join(x, cannot, by = c("doctor_id", "role"))
  }

  if (rule_on(inputs, "NIGHTS_EXEMPT")) {
    exempt <- staff$doctor_id[staff$nights_exempt %in% TRUE]
    x <- filter(x, !(is_night & doctor_id %in% exempt))
  }

  if (rule_on(inputs, "LTFT_FIXED_DAY")) {
    day_off <- staff |>
      filter(!is.na(ltft_fixed_day_off)) |>
      select(doctor_id, weekday = ltft_fixed_day_off)
    x <- x |>
      mutate(weekday = as.character(lubridate::wday(date, label = TRUE, locale = "C"))) |>
      anti_join(day_off, by = c("doctor_id", "weekday")) |>
      select(-weekday)
  }

  # As in the checker, a shift clashes with leave if any part of it falls on
  # a leave day, so no night the evening before leave starts.
  if (rule_on(inputs, "LEAVE_APPROVED")) {
    on_leave <- leave_days(inputs) |>
      filter(status == "approved") |>
      distinct(doctor_id, date)
    x <- x |>
      anti_join(on_leave, by = c("doctor_id", "date")) |>
      filter(!(end_h > 24 & paste(doctor_id, date + 1) %in% paste(on_leave$doctor_id, on_leave$date)))
  }

  x |>
    distinct(doctor_id, date, shift_code, .keep_all = TRUE) |>
    mutate(t = as.integer(date - inputs$period$start) + 1L) |>
    arrange(t, shift_code, doctor_id)
}

# One indicator per doctor and weekend counted by WEEKEND_MAX: the weekends
# (identified by their Saturday) that fall wholly within the doctor's post
counted_weekends <- function(inputs) {
  saturdays <- rota_dates(inputs)
  saturdays <- saturdays[lubridate::wday(saturdays, week_start = 1) == 6 &
                           saturdays + 1 <= inputs$period$end]

  inputs$staff |>
    select(doctor_id, start_date, end_date) |>
    tidyr::crossing(sat = saturdays) |>
    filter(sat >= start_date, sat + 1 <= end_date) |>
    select(doctor_id, sat)
}

# Weekends each shift variable touches: any time between 00:00 Saturday and
# 24:00 Sunday, so Friday nights count
weekend_touches <- function(x) {
  x |>
    select(doctor_id, date, start_h, end_h, x_j = j) |>
    mutate(sat = floor_saturday(date)) |>
    cross_join(tibble::tibble(next_sat = c(0, 7))) |>
    mutate(sat = sat + next_sat, k = as.numeric(date - sat)) |>
    filter(24 * k + start_h < 48, 24 * k + end_h > 0) |>
    select(doctor_id, sat, date, x_j)
}

# weekend[d, sat] = 1 if the doctor works any shift touching that weekend.
# `counted` marks the weekends WEEKEND_MAX and WEEKEND_TARGET count.
weekend_vars <- function(inputs, touches) {
  touches |>
    distinct(doctor_id, sat) |>
    left_join(mutate(counted_weekends(inputs), counted = TRUE), by = c("doctor_id", "sat")) |>
    mutate(counted = coalesce(counted, FALSE))
}

# Rules of the form "at most n consecutive days of <category>, then <rest>
# hours off". Nights are handled separately because their rest applies after
# a run of any length.
run_rule_table <- function(inputs, shifts) {
  rest <- rule_value(inputs, "REST_AFTER_RUN")
  tibble::tribble(
    ~rule,               ~category,
    "MAX_CONSEC_SHIFTS", rep(TRUE, nrow(shifts)),
    "MAX_CONSEC_LONG",   shifts$is_long & !shifts$is_night,
    "MAX_CONSEC_LATE",   shifts$is_late
  ) |>
    mutate(
      shift_codes = map(category, \(keep) shifts$shift_code[keep]),
      n = map_dbl(rule, \(id) rule_value(inputs, id)),
      rest = if (rule_on(inputs, "REST_AFTER_RUN")) rest else NA_real_
    ) |>
    filter(map_lgl(rule, \(id) rule_on(inputs, id)), lengths(shift_codes) > 0) |>
    select(rule, shift_codes, n, rest)
}

# Constraint families ---------------------------------------------------

# Bounds come from `lo`/`hi` columns if `df` has them, else the arguments
constraint <- function(df, rule, lo = -Inf, hi = Inf) {
  if (!"lo" %in% names(df)) df$lo <- lo
  if (!"hi" %in% names(df)) df$hi <- hi
  df |>
    mutate(rule = rule) |>
    select(rule, con, j, coef, lo, hi)
}

# Cost of each unfilled slot: more than any combination of soft penalties
# could save, so slots are left empty only when the hard rules leave no way
# to fill them
unfilled_weight <- 1e5

# Cover: n_min <= doctors on the shift <= n_max. A shortfall below n_min is
# allowed through `unfilled` slack at a very high cost, so impossible inputs
# still give a rota, with the slots that cannot be filled left empty, rather
# than no answer. Rest and hours rules have no slack: an empty slot is
# always possible, so they never need breaking.
cover_part <- function(x, inputs) {
  if (!rule_on(inputs, "COVER")) {
    return(NULL)
  }
  cover <- expected_cover(inputs) |>
    filter(n_max > 0) |>
    transmute(date, shift_code, n_min, n_max, con = paste(date, shift_code))
  unfilled <- cover |>
    filter(n_min > 0) |>
    mutate(aux = paste("COVER", con))
  weight <- coalesce(inputs$rules$weight[match("COVER", inputs$rules$rule_id)], unfilled_weight)

  list(
    vars = aux_vars(unfilled$aux, upper = unfilled$n_min),
    terms = bind_rows(
      x |>
        inner_join(cover, by = c("date", "shift_code")) |>
        transmute(con, j, coef = 1, lo = n_min, hi = n_max),
      transmute(unfilled, con, aux, coef = 1, lo = n_min, hi = n_max)
    ) |>
      mutate(rule = "COVER"),
    costs = transmute(unfilled, rule = "COVER", aux, coef = weight)
  )
}

one_shift_terms <- function(x) {
  x |>
    transmute(con = paste(doctor_id, t), j, coef = 1) |>
    constraint("ONE_SHIFT_PER_DAY", hi = 1)
}

# Shift variables on day t + k, for `keys` with columns con, doctor_id, t
# (already shifted) and shift_code
x_at <- function(keys, x) {
  keys |>
    inner_join(select(x, doctor_id, t, shift_code, j), by = c("doctor_id", "t", "shift_code"))
}

# For each shift s1 worked on day t, at most one of: s1, or a shift starting
# less than `limit` hours after s1 ends on day t + k. One row per k, because
# a doctor not working s1 may work shifts on both t + 1 and t + 2.
min_rest_terms <- function(x, shifts, limit) {
  pairs <- shift_gaps(shifts, k = 1:2) |>
    filter(gap < limit)

  first <- x |>
    select(doctor_id, t, s1 = shift_code, j) |>
    inner_join(pairs, by = "s1", relationship = "many-to-many") |>
    mutate(con = paste(doctor_id, t, s1, k))
  later <- first |>
    transmute(con, doctor_id, t = t + k, shift_code = s2) |>
    x_at(x)

  bind_rows(
    distinct(first, con, j) |> semi_join(later, by = "con"),
    select(later, con, j)
  ) |>
    mutate(coef = 1) |>
    constraint("MIN_REST", hi = 1)
}

# Hours in any 168 h. As in the checker, the worst window starts at a shift
# start, so windows starting at every shift start time on every day cover it.
max_7day_terms <- function(x, shifts, limit, n_days) {
  anchors <- unique(shifts$start_h %% 24)
  overlap <- tidyr::crossing(
    select(shifts, shift_code, start_h, end_h),
    tau = anchors,
    k = -1:7
  ) |>
    mutate(coef = pmin(24 * k + end_h, tau + 168) - pmax(24 * k + start_h, tau)) |>
    filter(coef > 0) |>
    select(shift_code, tau, k, coef)

  x |>
    select(doctor_id, t, shift_code, j) |>
    inner_join(overlap, by = "shift_code", relationship = "many-to-many") |>
    mutate(w = t - k) |>
    filter(w >= 1, w <= n_days) |>
    # Drop windows that cannot reach the limit
    filter(sum(coef) > limit, .by = c(doctor_id, w, tau)) |>
    transmute(con = paste(doctor_id, w, tau), j, coef) |>
    constraint("MAX_7DAY", hi = limit)
}

# Approved leave counts as a normal day's hours (see leave_hours()), so it
# uses up part of the limit
avg_hours_terms <- function(x, inputs) {
  limit <- inputs$staff |>
    transmute(
      doctor_id,
      hi = wte * weeks_in_post(inputs) * if_else(opted_out_wtr %in% TRUE,
                                                 rule_value(inputs, "AVG_HOURS_OPTOUT"),
                                                 rule_value(inputs, "AVG_HOURS"))
    ) |>
    left_join(leave_hours(inputs), by = "doctor_id") |>
    mutate(hi = hi - coalesce(leave_hours, 0))

  x |>
    inner_join(limit, by = "doctor_id") |>
    transmute(con = doctor_id, j, coef = hours, hi) |>
    constraint("AVG_HOURS")
}

# Weeks each doctor in the staff sheet is in post during the rota
weeks_in_post <- function(inputs) {
  staff <- inputs$staff
  pmax(as.numeric(pmin(staff$end_date, inputs$period$end) -
                    pmax(staff$start_date, inputs$period$start) + 1) / 7, 0)
}

# At most n days with a shift in `shift_codes` in any n + 1 consecutive days
window_terms <- function(x, shift_codes, n, n_days, rule) {
  x |>
    filter(shift_code %in% shift_codes) |>
    select(doctor_id, t, j) |>
    cross_join(tibble::tibble(back = 0:n)) |>
    mutate(w = t - back) |>
    filter(w >= 1, w + n <= n_days) |>
    transmute(con = paste(doctor_id, w), j, coef = 1) |>
    constraint(rule, hi = n)
}

# Max consecutive shifts / long shifts / long evenings, then rest. A run of
# the maximum n days ending on day t (more is ruled out by the window) must
# be followed by `rest` hours off. Grouping the last day's shifts by end
# time e, for each later day t + k:
#   sum(category days t - n + 1 ... t - 1) + category shifts on t ending at e
#   + shifts on t + k starting less than `rest` hours after e  <=  n
run_terms <- function(x, run_rules, shifts, n_days) {
  pmap(run_rules, \(rule, shift_codes, n, rest) {
    max_run <- window_terms(x, shift_codes, n, n_days, rule)
    if (is.na(rest)) {
      return(max_run)
    }

    in_run <- x |>
      filter(shift_code %in% shift_codes) |>
      select(doctor_id, t, end_h, j)
    pairs <- shift_gaps(shifts, k = 1:3) |>
      inner_join(select(shifts, s1 = shift_code, end_h), by = "s1") |>
      filter(s1 %in% shift_codes, gap < rest) |>
      distinct(end_h, k, shift_code = s2)

    last <- in_run |>
      filter(t >= n) |>
      inner_join(distinct(pairs, end_h, k), by = "end_h", relationship = "many-to-many") |>
      mutate(con = paste(rule, doctor_id, t, end_h, k))
    after <- last |>
      distinct(con, doctor_id, t, end_h, k) |>
      inner_join(pairs, by = c("end_h", "k"), relationship = "many-to-many") |>
      mutate(t = t + k) |>
      x_at(x)
    keys <- distinct(after, con, doctor_id, t = t - k)
    before <- keys |>
      cross_join(tibble::tibble(back = seq_len(n - 1))) |>
      transmute(con, doctor_id, t = t - back) |>
      inner_join(select(in_run, doctor_id, t, j), by = c("doctor_id", "t"),
                 relationship = "many-to-many")

    rest_after <- bind_rows(
      before,
      semi_join(last, keys, by = "con"),
      after
    ) |>
      transmute(con, j, coef = 1) |>
      constraint("REST_AFTER_RUN", hi = n)

    bind_rows(max_run, rest_after)
  }) |>
    list_rbind()
}

# Max consecutive nights, and rest after any run of nights: if night s ends
# a run on day t (no night on t + 1), no shift may start within `rest` hours
# of its end: x[t, s] - night[t + 1] + x[t + k, s'] <= 1.
night_terms <- function(x, shifts, inputs, n_days) {
  nights <- shifts$shift_code[shifts$is_night]
  if (length(nights) == 0) {
    return(NULL)
  }

  max_nights <- if (rule_on(inputs, "MAX_CONSEC_NIGHTS")) {
    window_terms(x, nights, rule_value(inputs, "MAX_CONSEC_NIGHTS"), n_days, "MAX_CONSEC_NIGHTS")
  }
  if (!rule_on(inputs, "REST_AFTER_NIGHTS")) {
    return(max_nights)
  }

  pairs <- shift_gaps(shifts, k = 1:3) |>
    filter(s1 %in% nights, gap < rule_value(inputs, "REST_AFTER_NIGHTS"),
           !(k == 1 & s2 %in% nights))
  last <- x |>
    filter(shift_code %in% nights) |>
    select(doctor_id, t, s1 = shift_code, j) |>
    inner_join(distinct(pairs, s1, k), by = "s1", relationship = "many-to-many") |>
    mutate(con = paste(doctor_id, t, s1, k))
  after <- last |>
    select(con, doctor_id, t, s1, k) |>
    inner_join(pairs, by = c("s1", "k"), relationship = "many-to-many") |>
    transmute(con, doctor_id, t = t + k, shift_code = s2) |>
    x_at(x)
  keep <- unique(after$con)
  next_night <- last |>
    filter(con %in% keep) |>
    distinct(con, doctor_id, t = t + 1) |>
    cross_join(tibble::tibble(shift_code = nights)) |>
    x_at(x)

  rest_after <- bind_rows(
    last |> filter(con %in% keep) |> transmute(con, j, coef = 1),
    transmute(next_night, con, j, coef = -1),
    transmute(after, con, j, coef = 1)
  ) |>
    constraint("REST_AFTER_NIGHTS", hi = 1)

  bind_rows(max_nights, rest_after)
}

# weekend[d, sat] is 1 exactly when the doctor works a shift touching the
# weekend: at least each day's shifts, at most all of them. At most 1 in
# `WEEKEND_MAX` counted weekends worked.
weekend_terms <- function(x, weekend, touches, inputs) {
  touches <- touches |>
    inner_join(select(weekend, doctor_id, sat, w_j = j), by = c("doctor_id", "sat"))

  at_least <- bind_rows(
    transmute(touches, con = paste(doctor_id, sat, date), j = x_j, coef = 1),
    touches |>
      distinct(doctor_id, sat, date, w_j) |>
      transmute(con = paste(doctor_id, sat, date), j = w_j, coef = -1)
  ) |>
    constraint("WEEKEND_WORKED", hi = 0)
  at_most <- bind_rows(
    transmute(touches, con = paste(doctor_id, sat), j = x_j, coef = -1),
    transmute(weekend, con = paste(doctor_id, sat), j, coef = 1)
  ) |>
    constraint("WEEKEND_WORKED_MAX", hi = 0)
  if (!rule_on(inputs, "WEEKEND_MAX")) {
    return(bind_rows(at_least, at_most))
  }

  one_in <- rule_value(inputs, "WEEKEND_MAX")
  frequency <- counted_weekends(inputs) |>
    count(doctor_id, name = "total") |>
    inner_join(filter(weekend, counted), by = "doctor_id") |>
    transmute(con = doctor_id, j, coef = 1, hi = floor(total / one_in)) |>
    constraint("WEEKEND_MAX")

  bind_rows(at_least, at_most, frequency)
}
