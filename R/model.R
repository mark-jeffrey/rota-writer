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
# column j), and `rows` the bounds lo <= row <= hi. Each constraint family
# below returns terms with columns rule, con (a key unique within the rule),
# j, coef, lo and hi. Rules switched off in the `rules` sheet are left out.

#' Build the rota optimisation model
#'
#' Turns the inputs into a mixed-integer linear programme with one binary
#' variable per doctor, day and shift that the doctor may work. Only hard
#' rules are modelled so far: cover, one shift per day, in post, competency,
#' placement, nights-exempt, LTFT fixed days, approved leave, and the TCS
#' Schedule 3 limits on hours, rest, consecutive shifts, nights and weekends.
#' Most users want [solve_rota()], which calls this.
#'
#' @inheritParams check_rota
#'
#' @returns A `rota_model`: a list of tibbles.
#'   \describe{
#'     \item{vars}{One row per variable: `j`, `kind` (`"x"` for doctor/day/
#'       shift, `"weekend"` for weekend-worked indicators), `type` (`"I"`
#'       or `"C"`), bounds and `cost`. Shift variables also have `doctor_id`,
#'       `date`, `t` and `shift_code`.}
#'     \item{rows}{One row per constraint: `i`, `rule` (matching the
#'       `rules` sheet where there is one), `con` and the bounds `lo`, `hi`.}
#'     \item{terms}{Non-zero coefficients: `i`, `j`, `coef`.}
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

  x <- shift_vars(inputs, shifts)
  check_fillable(x, inputs)

  run_rules <- run_rule_table(inputs, shifts)
  vars <- bind_rows(
    mutate(x, kind = "x", type = "I"),
    weekend_vars(inputs) |> mutate(kind = "weekend", type = "C")
  ) |>
    mutate(j = row_number(), lower = 0, upper = 1, cost = 0)
  x <- filter(vars, kind == "x")

  terms <- list(
    cover_terms(x, inputs),
    one_shift_terms(x),
    if (rule_on(inputs, "MIN_REST")) min_rest_terms(x, shifts, rule_value(inputs, "MIN_REST")),
    if (rule_on(inputs, "MAX_7DAY")) max_7day_terms(x, shifts, rule_value(inputs, "MAX_7DAY"), n_days),
    if (rule_on(inputs, "AVG_HOURS")) avg_hours_terms(x, inputs),
    run_terms(x, run_rules, shifts, n_days),
    night_terms(x, shifts, inputs, n_days),
    if (rule_on(inputs, "WEEKEND_MAX")) weekend_terms(x, vars, rule_value(inputs, "WEEKEND_MAX"))
  ) |>
    list_rbind() |>
    mutate(i = cur_group_id(), .by = c(rule, con))

  rows <- terms |>
    distinct(i, rule, con, lo, hi) |>
    arrange(i)
  stopifnot(!anyDuplicated(rows$i))

  structure(
    list(vars = vars, rows = rows, terms = select(terms, i, j, coef)),
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

  if (rule_on(inputs, "PLACEMENT")) {
    allowed <- inputs$placements |>
      inner_join(inputs$placement_roles, by = "placement", relationship = "many-to-many") |>
      select(doctor_id, role, p_start = start_date, p_end = end_date)
    x <- semi_join(x, allowed, join_by(doctor_id, role, between(date, p_start, p_end)))
  }

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

# Fail early, with a useful message, if a slot has too few eligible doctors
check_fillable <- function(x, inputs) {
  if (!rule_on(inputs, "COVER")) {
    return(invisible())
  }
  short <- expected_cover(inputs) |>
    filter(n_min > 0) |>
    left_join(count(x, date, shift_code), by = c("date", "shift_code")) |>
    mutate(n = coalesce(n, 0L)) |>
    filter(n < n_min)
  if (nrow(short) > 0) {
    shown <- head(short, 10)
    stop("Some shifts have fewer eligible doctors than they need ",
         "(check competencies, placements, leave and LTFT days):\n",
         paste0("  ", shown$date, " ", shown$shift_code, ": ", shown$n, " eligible, needs ",
                shown$n_min, collapse = "\n"),
         if (nrow(short) > 10) paste0("\n  ... and ", nrow(short) - 10, " more"),
         call. = FALSE)
  }
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

weekend_vars <- function(inputs) {
  if (!rule_on(inputs, "WEEKEND_MAX")) {
    return(NULL)
  }
  counted_weekends(inputs)
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

cover_terms <- function(x, inputs) {
  if (!rule_on(inputs, "COVER")) {
    return(NULL)
  }
  x |>
    inner_join(select(expected_cover(inputs), date, shift_code, n_min, n_max),
               by = c("date", "shift_code")) |>
    transmute(con = paste(date, shift_code), j, coef = 1, lo = n_min, hi = n_max) |>
    constraint("COVER")
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

avg_hours_terms <- function(x, inputs) {
  limit <- inputs$staff |>
    transmute(
      doctor_id,
      weeks = as.numeric(pmin(end_date, inputs$period$end) - pmax(start_date, inputs$period$start) + 1) / 7,
      hi = wte * weeks * if_else(opted_out_wtr %in% TRUE,
                                 rule_value(inputs, "AVG_HOURS_OPTOUT"),
                                 rule_value(inputs, "AVG_HOURS"))
    )

  x |>
    inner_join(limit, by = "doctor_id") |>
    transmute(con = doctor_id, j, coef = hours, hi) |>
    constraint("AVG_HOURS")
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

# weekend[d, sat] >= any shift touching 00:00 Saturday to 24:00 Sunday
# (so Friday nights count), and at most 1 in `one_in` counted weekends worked
weekend_terms <- function(x, vars, one_in) {
  weekend <- vars |>
    filter(kind == "weekend") |>
    select(doctor_id, sat, w_j = j)

  touches <- x |>
    select(doctor_id, date, start_h, end_h, j) |>
    mutate(sat = floor_saturday(date)) |>
    cross_join(tibble::tibble(next_sat = c(0, 7))) |>
    mutate(sat = sat + next_sat, k = as.numeric(date - sat)) |>
    filter(24 * k + start_h < 48, 24 * k + end_h > 0) |>
    inner_join(weekend, by = c("doctor_id", "sat"))

  worked <- bind_rows(
    transmute(touches, con = paste(doctor_id, sat, date), j, coef = 1),
    touches |> distinct(doctor_id, sat, date, w_j) |>
      transmute(con = paste(doctor_id, sat, date), j = w_j, coef = -1)
  ) |>
    constraint("WEEKEND_WORKED", hi = 0)

  frequency <- weekend |>
    mutate(hi = floor(n() / one_in), .by = doctor_id) |>
    transmute(con = doctor_id, j = w_j, coef = 1, hi) |>
    constraint("WEEKEND_MAX")

  bind_rows(worked, frequency)
}
