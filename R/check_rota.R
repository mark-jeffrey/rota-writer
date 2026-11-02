# Independent compliance checker.
#
# Works on real clock times: each rostered shift becomes an interval
# (a night starting 20:00 ends 08:30 the next day). It deliberately does not
# reuse the optimiser's day-index formulation, so the two can catch each
# other's mistakes. Times are wall-clock (no BST adjustment).

#' Check a rota against the contract and local rules
#'
#' Checks every rostered shift against the hard and soft rules in the
#' `rules` sheet of the inputs workbook: TCS Schedule 3 limits (hours, rest,
#' consecutive shifts, nights, weekends), cover, competencies, placements,
#' LTFT days, leave, requests and skill mix. It works on real clock times, so
#' a night starting 20:00 ends 08:30 the next day and clashes with leave
#' starting that morning.
#'
#' The checker is independent of the optimiser, so use it on any rota:
#' solved, built by hand, or edited after solving.
#'
#' @param rota The rota, in long format: a data frame with one row per
#'   doctor per working day and columns `doctor_id`, `date` and `code`, as
#'   returned by [read_rota()]. Alternatively, a path to a rota workbook with
#'   a `long` sheet. Codes that are not shifts (e.g. leave codes such as
#'   `AL`) are ignored.
#' @param inputs The rota inputs, as returned by [read_rota_inputs()], or a
#'   path to an inputs workbook.
#'
#' @returns A tibble with one row per violation, hard rules first:
#'   \describe{
#'     \item{rule_id}{Rule broken, matching the `rules` sheet (e.g.
#'       `MIN_REST`).}
#'     \item{hard_soft}{`"hard"` (must never happen) or `"soft"` (penalised).}
#'     \item{doctor_id}{Doctor concerned, or `NA` for department-level rules
#'       such as `COVER` and `SKILL_MIX`.}
#'     \item{start_date, end_date}{Dates the violation spans.}
#'     \item{detail}{Plain-English description.}
#'     \item{penalty}{Weighted penalty for soft rules; `NA` for hard rules.}
#'   }
#'   Rules switched off in the `rules` sheet (`enabled = N`) are not
#'   reported. Zero rows means the rota is fully compliant.
#'
#' @seealso [summarise_doctors()] for per-doctor totals and fair shares,
#'   [write_check_report()] to save the results as a workbook.
#' @export
#' @examples
#' inputs <- read_rota_inputs(system.file("extdata", "rota_inputs.xlsx", package = "rotawriter"))
#' rota <- read_rota(system.file("extdata", "example_rota_output.xlsx", package = "rotawriter"))
#'
#' violations <- check_rota(rota, inputs)
#' dplyr::count(violations, hard_soft, rule_id)
#'
#' # File paths work too
#' check_rota(
#'   system.file("extdata", "example_rota_output.xlsx", package = "rotawriter"),
#'   system.file("extdata", "rota_inputs.xlsx", package = "rotawriter")
#' )
check_rota <- function(rota, inputs) {
  if (missing(inputs)) {
    stop("check_rota() needs both the rota and the inputs, e.g.\n",
         "  check_rota(rota, read_rota_inputs(\"rota_inputs.xlsx\"))", call. = FALSE)
  }
  inputs <- as_rota_inputs(inputs)
  si <- shift_instances(as_rota(rota), inputs)

  checks <- compact(map(active_rules(inputs), "check"))

  map(checks, \(check) check(si, inputs)) |>
    list_rbind() |>
    finalise_violations(inputs)
}

# One row per rostered shift, with start/end datetimes and shift flags
shift_instances <- function(rota, inputs) {
  rota |>
    mutate(date = as.Date(date)) |>
    inner_join(inputs$shifts, by = c(code = "shift_code")) |>
    mutate(
      start_dt = as.POSIXct(paste(date, start), tz = "UTC"),
      end_dt   = start_dt + hours * 3600
    ) |>
    arrange(doctor_id, start_dt)
}

violations <- function(rule_id, doctor_id = NA_character_, start_date, end_date = start_date,
                       detail, weight = NA_real_) {
  tibble::tibble(rule_id, doctor_id, start_date, end_date, detail, weight)
}

rule_value <- function(inputs, id) {
  inputs$rules$value[inputs$rules$rule_id == id]
}

finalise_violations <- function(v, inputs) {
  rules <- inputs$rules |>
    select(rule_id, hard_soft, rule_weight = weight)

  v |>
    left_join(rules, by = "rule_id") |>
    filter(map_lgl(rule_id, \(id) rule_on(inputs, id))) |>
    mutate(
      hard_soft = coalesce(hard_soft, "hard"),
      penalty = if_else(hard_soft == "soft", coalesce(weight, 1) * coalesce(rule_weight, 1), NA_real_)
    ) |>
    arrange(desc(hard_soft == "hard"), rule_id, start_date, doctor_id) |>
    select(rule_id, hard_soft, doctor_id, start_date, end_date, detail, penalty)
}

# Department-level rules -------------------------------------------------

# Cover slots (shift codes in the cover sheet) each day. Slots not listed
# for a day type are not needed; a blank n_max means no maximum (e.g. normal
# days).
expected_cover <- function(inputs) {
  tidyr::crossing(date = rota_dates(inputs), shift_code = unique(shift_covers(inputs$shifts)$slot)) |>
    mutate(day_type = day_type(date, inputs)) |>
    left_join(mutate(inputs$cover, listed = TRUE), by = c("shift_code", "day_type")) |>
    mutate(
      n_min = coalesce(n_min, 0),
      n_max = if_else(listed %in% TRUE, coalesce(n_max, Inf), 0)
    ) |>
    select(-listed)
}

# The cover slots each shift fills: its `covers` (shift codes separated by
# ";"), or else itself. A 24-hour shift covering "C2;CN" fills a 2nd-on day
# and a night.
shift_covers <- function(shifts) {
  covers <- shifts$covers %||% rep(NA_character_, nrow(shifts))
  tibble::tibble(shift_code = shifts$shift_code,
                 slot = stringr::str_split(coalesce(covers, shifts$shift_code), ";")) |>
    unnest(slot) |>
    mutate(slot = stringr::str_trim(slot))
}

# A rota (with a `code` column) with one row per slot each shift fills
rota_slots <- function(rota, inputs) {
  inner_join(rota, shift_covers(inputs$shifts), by = c(code = "shift_code"),
             relationship = "many-to-many")
}

check_cover <- function(si, inputs) {
  actual <- count(rota_slots(si, inputs), date, shift_code = slot)

  expected_cover(inputs) |>
    left_join(actual, by = c("date", "shift_code")) |>
    mutate(n = coalesce(n, 0L)) |>
    filter(n < n_min | n > n_max) |>
    with(violations(
      "COVER",
      start_date = date,
      detail = sprintf("%s: %d rostered, needs %s", shift_code, n,
                       case_when(n_min == n_max ~ as.character(n_min),
                                 is.infinite(n_max) ~ paste("at least", n_min),
                                 .default = paste0(n_min, "-", n_max)))
    ))
}

check_skill_mix <- function(si, inputs) {
  rank_of <- set_names(inputs$grades$rank, inputs$grades$grade)

  sm <- inputs$skill_mix |>
    transmute(sm_id = rule_id, description, min_rank = rank_of[min_grade],
              min_count, weight, shift_code = stringr::str_split(shifts, ";")) |>
    unnest(shift_code)

  # Days the rule applies: any of its shifts is required that day
  applies <- expected_cover(inputs) |>
    filter(n_min > 0) |>
    inner_join(sm, by = "shift_code", relationship = "many-to-many") |>
    distinct(sm_id, description, min_rank, min_count, weight, date)

  seniors <- si |>
    left_join(select(inputs$staff, doctor_id, grade), by = "doctor_id") |>
    mutate(rank = rank_of[grade]) |>
    inner_join(sm, by = c(code = "shift_code"), relationship = "many-to-many") |>
    filter(rank >= min_rank) |>
    distinct(sm_id, date, doctor_id) |>
    count(sm_id, date, name = "n_senior")

  applies |>
    left_join(seniors, by = c("sm_id", "date")) |>
    mutate(n_senior = coalesce(n_senior, 0L)) |>
    filter(n_senior < min_count) |>
    with(violations(
      "SKILL_MIX",
      start_date = date,
      detail = sprintf("%s: %s (%d found)", sm_id, description, n_senior),
      weight = weight
    ))
}

# Who may work what ------------------------------------------------------

check_in_post <- function(si, inputs) {
  si |>
    left_join(select(inputs$staff, doctor_id, from = start_date, to = end_date), by = "doctor_id") |>
    filter(is.na(from) | date < from | date > to) |>
    with(violations(
      "IN_POST", doctor_id, date,
      detail = if_else(is.na(from), paste(code, "- doctor not in staff list"),
                       paste(code, "- not in post on this date"))
    ))
}

# One row per doctor and role in the staff sheet's can_* columns (none if
# the sheet has no such columns)
staff_competencies <- function(inputs) {
  if (!any(startsWith(names(inputs$staff), "can_"))) {
    return(tibble::tibble(doctor_id = character(), role = character(), can = logical()))
  }
  inputs$staff |>
    select(doctor_id, starts_with("can_")) |>
    pivot_longer(-doctor_id, names_to = "role", names_prefix = "can_", values_to = "can")
}

check_competency <- function(si, inputs) {
  can <- staff_competencies(inputs)

  si |>
    inner_join(can, by = c("doctor_id", "role")) |>
    filter(!can) |>
    with(violations("COMPETENCY", doctor_id, date,
                    detail = paste(code, "- not signed off for", role)))
}

check_placement <- function(si, inputs) {
  placements <- inputs$placements |>
    select(doctor_id, placement, p_start = start_date, p_end = end_date)

  checked <- si |>
    left_join(placements, join_by(doctor_id, between(date, p_start, p_end))) |>
    left_join(inputs$placement_roles, by = c("placement", "role"))

  bind_rows(
    checked |>
      filter(is.na(placement) | is.na(penalty)) |>
      with(violations(
        "PLACEMENT", doctor_id, date,
        detail = if_else(is.na(placement), paste(code, "- no placement on this date"),
                         paste0(code, " - ", role, " not allowed in ", placement, " block"))
      )),
    checked |>
      filter(penalty > 0) |>
      with(violations("PLACEMENT_AVOID", doctor_id, date,
                      detail = paste0(code, " - ", role, " discouraged in ", placement, " block"),
                      weight = penalty))
  )
}

# Shifts open only to doctors who prefer them (`open_to` = "preference" in
# the shifts sheet), as (doctor_id, shift_code) pairs barred
preference_only_shifts <- function(inputs) {
  preferred <- inputs$preferences |>
    filter(preference == "prefer_shift") |>
    distinct(doctor_id, shift_code = value)
  inputs$shifts |>
    filter(tolower(coalesce(open_to, "all")) == "preference") |>
    select(shift_code) |>
    tidyr::crossing(doctor_id = inputs$staff$doctor_id) |>
    anti_join(preferred, by = c("doctor_id", "shift_code"))
}

check_preference_only <- function(si, inputs) {
  si |>
    semi_join(preference_only_shifts(inputs), by = c("doctor_id", code = "shift_code")) |>
    with(violations("PREFERENCE_ONLY", doctor_id, date,
                    detail = paste(code, "- only for doctors who prefer it")))
}

check_nights_exempt <- function(si, inputs) {
  exempt <- inputs$staff$doctor_id[inputs$staff$nights_exempt]

  si |>
    filter(is_night, doctor_id %in% exempt) |>
    with(violations("NIGHTS_EXEMPT", doctor_id, date, detail = paste(code, "- doctor is nights-exempt")))
}

check_ltft_day <- function(si, inputs) {
  si |>
    inner_join(select(inputs$staff, doctor_id, day_off = ltft_fixed_day_off), by = "doctor_id") |>
    filter(!is.na(day_off),
           as.character(lubridate::wday(date, label = TRUE, locale = "C")) == day_off) |>
    with(violations("LTFT_FIXED_DAY", doctor_id, date,
                    detail = paste(code, "- on fixed non-working day", day_off)))
}

leave_days <- function(inputs) {
  inputs$leave |>
    mutate(date = map2(start_date, end_date, \(s, e) seq(s, e, by = "day"))) |>
    unnest(date, ptype = list(date = as.Date(character()))) |>
    distinct(doctor_id, date, leave_type, status)
}

# A shift clashes with leave if any part of it falls on a leave day, so a
# night finishing on the first morning of leave counts.
check_leave <- function(si, inputs) {
  bind_rows(
    mutate(si, leave_date = date),
    si |> filter(end_dt > as.POSIXct(date + 1)) |> mutate(leave_date = date + 1)
  ) |>
    inner_join(leave_days(inputs), by = c("doctor_id", leave_date = "date"),
               relationship = "many-to-many") |>
    # One row per shift, approved leave taking precedence
    arrange(status != "approved") |>
    distinct(doctor_id, date, code, .keep_all = TRUE) |>
    with(violations(
      if_else(status == "approved", "LEAVE_APPROVED", "LEAVE_REQUESTED"),
      doctor_id, date, leave_date,
      detail = paste0(code, if_else(leave_date == date, " - on ", " - finishes on "),
                      status, " ", leave_type, " leave")
    ))
}

# Hours and rest ---------------------------------------------------------

check_max_shift <- function(si, inputs) {
  limit <- rule_value(inputs, "MAX_SHIFT")

  si |>
    filter(hours > limit) |>
    with(violations("MAX_SHIFT", doctor_id, date,
                    detail = sprintf("%s is %.1f h (max %g)", code, hours, limit)))
}

check_min_rest <- function(si, inputs) {
  limit <- rule_value(inputs, "MIN_REST")

  si |>
    mutate(
      prev_code = lag(code), prev_date = lag(date),
      gap = as.numeric(difftime(start_dt, lag(end_dt), units = "hours")),
      .by = doctor_id
    ) |>
    filter(!is.na(gap), gap < limit) |>
    with(violations(
      "MIN_REST", doctor_id, prev_date, date,
      detail = if_else(gap < 0,
                       sprintf("%s overlaps %s", code, prev_code),
                       sprintf("%.1f h rest between %s and %s (min %g)", gap, prev_code, code, limit))
    ))
}

# Max hours in any 168 h window. The worst window always starts at a shift
# start, so it is enough to test windows anchored there.
check_max_7day <- function(si, inputs) {
  limit <- rule_value(inputs, "MAX_7DAY")
  shifts <- select(si, doctor_id, start_dt, end_dt)

  breaches <- si |>
    select(doctor_id, w_date = date, w_start = start_dt) |>
    inner_join(shifts, by = "doctor_id", relationship = "many-to-many") |>
    mutate(
      w_end = w_start + 168 * 3600,
      overlap = pmax(0, as.numeric(difftime(pmin(end_dt, w_end), pmax(start_dt, w_start), units = "hours")))
    ) |>
    summarise(hours = sum(overlap), .by = c(doctor_id, w_date, w_start)) |>
    filter(hours > limit)
  if (nrow(breaches) == 0) {
    return(NULL)
  }

  breaches |>
    arrange(doctor_id, w_start) |>
    # Overlapping breaching windows are one breach
    mutate(breach = cumsum(c(TRUE, diff(w_date) > 7)), .by = doctor_id) |>
    summarise(from = min(w_date), to = max(w_date) + 6, hours = max(hours),
              .by = c(doctor_id, breach)) |>
    with(violations("MAX_7DAY", doctor_id, from, to,
                    detail = sprintf("%.1f h in 168 h (max %g)", hours, limit)))
}

# Approved leave on a day the doctor would otherwise work (a weekday that is
# not a bank holiday or their LTFT day off, while in post) counts as a
# normal day's hours for average and contracted hours
leave_hours <- function(inputs) {
  normal_day <- inputs$shifts$hours[inputs$shifts$shift_type == "normal_day"]
  day_hours <- if (length(normal_day) > 0) max(normal_day) else 0

  leave_days(inputs) |>
    filter(status == "approved") |>
    distinct(doctor_id, date) |>
    semi_join(in_post_days(inputs), by = c("doctor_id", "date")) |>
    filter(day_type(date, inputs) == "weekday") |>
    left_join(select(inputs$staff, doctor_id, day_off = ltft_fixed_day_off), by = "doctor_id") |>
    filter(is.na(day_off) | as.character(lubridate::wday(date, label = TRUE, locale = "C")) != day_off) |>
    count(doctor_id, name = "leave_days") |>
    mutate(leave_hours = leave_days * day_hours) |>
    select(doctor_id, leave_hours)
}

# Fairness pool of each doctor in the staff sheet. Fair shares are split
# within a pool, so doctors in separate pools (e.g. anaesthetics and an ICU
# tier 2 rota) are not compared. A blank pool, or no pool column, is one
# shared pool.
staff_pool <- function(inputs) {
  pool <- inputs$staff$pool %||% rep(NA_character_, nrow(inputs$staff))
  coalesce(as.character(pool), "")
}

# Weekly contracted hours: the staff sheet's contracted_hours, or the
# HOURS_TARGET rule's value x WTE where that is blank
contracted_hours <- function(inputs) {
  given <- inputs$staff$contracted_hours %||% rep(NA_real_, nrow(inputs$staff))
  default <- rule_value(inputs, "HOURS_TARGET")
  if (length(default) == 0) default <- NA_real_
  coalesce(as.numeric(given), default * inputs$staff$wte)
}

check_avg_hours <- function(si, inputs) {
  limit <- rule_value(inputs, "AVG_HOURS")
  limit_optout <- rule_value(inputs, "AVG_HOURS_OPTOUT")

  doctor_summary(si, inputs) |>
    mutate(
      limit = if_else(opted_out_wtr %in% TRUE, limit_optout, limit) * wte,
      # Unrounded, so 48.04 h is not passed as 48.0
      avg_weekly_hours = (hours + leave_hours) / weeks_in_post
    ) |>
    filter(avg_weekly_hours > limit + 1e-9) |>
    with(violations(
      "AVG_HOURS", doctor_id, from, to,
      detail = sprintf("Average %.1f h/week over %.1f weeks (max %.1f at WTE %g)",
                       avg_weekly_hours, weeks_in_post, limit, wte)
    ))
}

# Hours (rostered plus leave) against contracted hours x weeks in post,
# scored as hours_target_part() does: the k-th normal day's worth of hours
# short costs (2k - 1) per hour (from the 4th day on, 7 per hour), and each
# hour over costs 10
check_hours_target <- function(si, inputs) {
  normal_day <- inputs$shifts$hours[inputs$shifts$shift_type == "normal_day"]
  day_hours <- if (length(normal_day) > 0) max(normal_day) else 8
  short_cost <- function(short) {
    steps <- pmin(pmax(short - day_hours * 0:2, 0), day_hours)
    steps[1] + 3 * steps[2] + 5 * steps[3] + 7 * max(short - 3 * day_hours, 0)
  }

  doctor_summary(si, inputs) |>
    filter(!is.na(contracted_hours), weeks_in_post > 0) |>
    mutate(diff = contracted_hours * weeks_in_post - hours - leave_hours) |>
    filter(abs(diff) > 1e-9) |>
    mutate(
      weight = if_else(diff > 0, map_dbl(diff, short_cost), -10 * diff),
      detail = sprintf("%.1f h %s contracted %.1f h x %.1f weeks (average %.1f h/week)",
                       abs(diff), if_else(diff > 0, "short of", "over"),
                       contracted_hours, weeks_in_post,
                       (hours + leave_hours) / weeks_in_post)
    ) |>
    with(violations("HOURS_TARGET", doctor_id, from, to, detail = detail, weight = weight))
}

# Each day's count of each normal-day shift against the median for that
# shift and day type over the rota (the level normal_day_spread_part()
# settles on)
check_normal_day_spread <- function(si, inputs) {
  normal <- inputs$shifts$shift_code[inputs$shifts$shift_type == "normal_day"]

  expected_cover(inputs) |>
    filter(shift_code %in% normal, n_max > 0) |>
    left_join(count(si, date, shift_code = code), by = c("date", "shift_code")) |>
    mutate(n = coalesce(n, 0L), level = median(n), .by = c(shift_code, day_type)) |>
    filter(n != level) |>
    with(violations("NORMAL_DAY_SPREAD", start_date = date, weight = abs(n - level),
                    detail = sprintf("%s: %d rostered (typical %g)", shift_code, n, level)))
}

# Runs of shifts on consecutive days -------------------------------------

# Collapse shifts matching `keep` into runs of consecutive days, with the
# rest from the end of each run to the doctor's next shift of any kind.
day_runs <- function(si, keep) {
  next_starts <- select(si, doctor_id, next_start = start_dt)
  kept <- filter(si, {{ keep }})
  if (nrow(kept) == 0) {
    return(tibble::tibble(doctor_id = character(), first = as.Date(character()),
                          last = as.Date(character()), n = integer(), rest_after = numeric()))
  }

  kept |>
    summarise(last_end = max(end_dt), .by = c(doctor_id, date)) |>
    arrange(doctor_id, date) |>
    mutate(run = cumsum(c(TRUE, diff(date) != 1)), .by = doctor_id) |>
    summarise(first = min(date), last = max(date), n = n(), last_end = max(last_end),
              .by = c(doctor_id, run)) |>
    left_join(next_starts, join_by(doctor_id, closest(last_end <= next_start))) |>
    mutate(rest_after = as.numeric(difftime(next_start, last_end, units = "hours")))
}

check_consecutive <- function(si, inputs) {
  rest <- rule_value(inputs, "REST_AFTER_RUN")

  run_rules <- list(
    MAX_CONSEC_SHIFTS = day_runs(si, TRUE),
    MAX_CONSEC_LONG   = day_runs(si, is_long & !is_night), # nights have their own rules
    MAX_CONSEC_LATE   = day_runs(si, is_late)
  )

  on <- map_lgl(names(run_rules), \(id) rule_on(inputs, id))
  imap(run_rules[on], \(runs, id) {
    max_n <- rule_value(inputs, id)
    what <- c(MAX_CONSEC_SHIFTS = "shifts", MAX_CONSEC_LONG = "long shifts",
              MAX_CONSEC_LATE = "long evening shifts")[[id]]
    bind_rows(
      runs |>
        filter(n > max_n) |>
        with(violations(id, doctor_id, first, last,
                        detail = sprintf("%d consecutive %s (max %g)", n, what, max_n))),
      runs |>
        filter(n == max_n, rest_after < (if (length(rest) == 1) rest else -Inf)) |>
        with(violations("REST_AFTER_RUN", doctor_id, first, last,
                        detail = sprintf("%.1f h rest after %d consecutive %s (min %g)",
                                         rest_after, n, what, rest)))
    )
  }) |>
    list_rbind() |>
    bind_rows(if (rule_on(inputs, "LONG_RUN")) {
      run_target(run_rules$MAX_CONSEC_SHIFTS, inputs, "LONG_RUN", "shifts")
    })
}

# Soft: each day of a run beyond the rule's value costs the weight
run_target <- function(runs, inputs, id, what) {
  target <- rule_value(inputs, id)
  if (length(target) == 0 || is.na(target)) {
    return(NULL)
  }
  runs |>
    filter(n > target) |>
    with(violations(id, doctor_id, first, last, weight = n - target,
                    detail = sprintf("%d consecutive %s (target %g)", n, what, target)))
}

check_nights <- function(si, inputs) {
  max_n <- rule_value(inputs, "MAX_CONSEC_NIGHTS")
  rest <- rule_value(inputs, "REST_AFTER_NIGHTS")
  runs <- day_runs(si, is_night)

  bind_rows(
    if (rule_on(inputs, "MAX_CONSEC_NIGHTS")) {
      runs |>
        filter(n > max_n) |>
        with(violations("MAX_CONSEC_NIGHTS", doctor_id, first, last,
                        detail = sprintf("%d consecutive nights (max %g)", n, max_n)))
    },
    if (rule_on(inputs, "REST_AFTER_NIGHTS")) {
      runs |>
        filter(rest_after < rest) |>
        with(violations("REST_AFTER_NIGHTS", doctor_id, first, last,
                        detail = sprintf("%.1f h rest after nights (min %g)", rest_after, rest)))
    },
    runs |>
      filter(n == 1) |>
      with(violations("SINGLE_NIGHT", doctor_id, first, last, detail = "Isolated single night")),
    run_target(runs, inputs, "LONG_NIGHT_RUN", "nights")
  )
}

# Soft rules on the shape of each doctor's pattern: ICU runs too short for
# continuity, single days off between working days, and split weekends
check_wellbeing <- function(si, inputs) {
  short_n <- rule_value(inputs, "ICU_SHORT_RUN")
  short <- if (length(short_n) == 1 && !is.na(short_n)) {
    short_icu_runs(si, inputs, short_n) |>
      with(violations("ICU_SHORT_RUN", doctor_id, first, last, weight = short_n - n,
                      detail = sprintf("%d-day ICU run (target at least %g)", n, short_n)))
  }
  day_off <- isolated_days_off(si, inputs) |>
    with(violations("ISOLATED_DAY_OFF", doctor_id, date, detail = "Single day off between shifts"))
  split <- split_weekends(si, inputs) |>
    with(violations("SPLIT_WEEKEND", doctor_id, sat, sat + 1, detail = "Works one day of the weekend"))

  # Rules added after older workbooks were made are skipped if missing
  filter(bind_rows(short, day_off, split), rule_id %in% inputs$rules$rule_id)
}

# Runs of ICU day shifts shorter than n, with a day outside the run on each
# side inside the rota period
short_icu_runs <- function(si, inputs, n) {
  day_runs(si, !is_night & stringr::str_starts(role, "ICU")) |>
    filter(n < .env$n, first > inputs$period$start, last < inputs$period$end)
}

# Days with no shift and no leave between two working days
isolated_days_off <- function(si, inputs) {
  worked <- distinct(si, doctor_id, date)
  worked |>
    mutate(date = date + 1) |>
    filter(date < inputs$period$end) |>
    semi_join(mutate(worked, date = date - 1), by = c("doctor_id", "date")) |>
    anti_join(worked, by = c("doctor_id", "date")) |>
    anti_join(distinct(leave_days(inputs), doctor_id, date), by = c("doctor_id", "date"))
}

# Weekends (by their Saturday) with working time on only one of the two
# days. A night counts on the morning it ends too.
split_weekends <- function(si, inputs) {
  period <- inputs$period
  bind_rows(
    select(si, doctor_id, day = date),
    si |> filter(end_dt > as.POSIXct(date + 1)) |> transmute(doctor_id, day = date + 1)
  ) |>
    mutate(sat = floor_saturday(day)) |>
    filter(day <= sat + 1, sat >= period$start, sat + 1 <= period$end) |>
    distinct(doctor_id, sat, day) |>
    count(doctor_id, sat) |>
    filter(n == 1) |>
    select(doctor_id, sat)
}

# Weekends ---------------------------------------------------------------

# Weekends (identified by their Saturday) in which each doctor works any
# time between 00:00 Saturday and 24:00 Sunday, so Friday nights count.
weekends_worked <- function(si) {
  si |>
    mutate(sat = floor_saturday(date)) |>
    reframe(sat = c(sat, sat + 7), .by = c(doctor_id, start_dt, end_dt)) |>
    mutate(we_start = as.POSIXct(sat), we_end = we_start + 48 * 3600) |>
    filter(start_dt < we_end, end_dt > we_start) |>
    distinct(doctor_id, sat)
}

floor_saturday <- function(date) {
  lubridate::floor_date(date, "week", week_start = 6)
}

check_weekends <- function(si, inputs) {
  hard_n <- rule_value(inputs, "WEEKEND_MAX")
  target_n <- rule_value(inputs, "WEEKEND_TARGET")

  saturdays <- rota_dates(inputs)
  saturdays <- saturdays[lubridate::wday(saturdays, week_start = 1) == 6 &
                           saturdays + 1 <= inputs$period$end]

  freq <- inputs$staff |>
    tidyr::crossing(sat = saturdays) |>
    filter(sat >= start_date, sat + 1 <= end_date) |>
    left_join(mutate(weekends_worked(si), worked = TRUE), by = c("doctor_id", "sat")) |>
    summarise(total = n(), worked = sum(!is.na(worked)),
              from = min(sat), to = max(sat) + 1, .by = doctor_id) |>
    mutate(detail = sprintf("Worked %d of %d weekends", worked, total))

  # Frequency over the doctor's weekends in post, as in TCS Schedule 3
  bind_rows(
    if (rule_on(inputs, "WEEKEND_MAX")) {
      freq |>
        filter(worked > total / hard_n) |>
        with(violations("WEEKEND_MAX", doctor_id, from, to,
                        detail = paste0(detail, " (more than 1 in ", hard_n, ")")))
    },
    if (rule_on(inputs, "WEEKEND_TARGET")) {
      freq |>
        mutate(excess = worked - floor(total / target_n)) |>
        filter(excess > 0) |>
        with(violations("WEEKEND_TARGET", doctor_id, from, to,
                        detail = paste0(detail, " (", excess, " more than 1 in ", target_n, ")"),
                        weight = excess))
    }
  )
}

# Requests ---------------------------------------------------------------

check_requests <- function(si, inputs) {
  day_flags <- si |>
    summarise(on_call = any(is_on_call), night = any(is_night), .by = c(doctor_id, date))
  weekends <- mutate(weekends_worked(si), weekend = TRUE)

  inputs$requests |>
    mutate(sat = floor_saturday(date)) |>
    left_join(day_flags, by = c("doctor_id", "date")) |>
    left_join(weekends, by = c("doctor_id", "sat")) |>
    mutate(across(c(on_call, night, weekend), \(x) coalesce(x, FALSE))) |>
    filter(case_when(
      request == "no_oncall"     ~ on_call,
      request == "no_night"      ~ night,
      request == "no_weekend"    ~ weekend,
      request == "prefer_oncall" ~ !on_call,
      .default = FALSE
    )) |>
    with(violations("REQUESTS", doctor_id, date,
                    detail = paste("Request not met:", request), weight = weight))
}

# Per-doctor summary -----------------------------------------------------

#' Summarise hours, on-calls, nights and weekends per doctor
#'
#' Totals for each doctor in the staff list, alongside the fair share they
#' would have if on-calls, nights and weekends were split in proportion to
#' `wte * weeks_in_post`. Shares are split within each staffing pool (the
#' `staff` sheet's `pool` column), so doctors are compared only with others
#' on the same rota. Use it to judge how even a rota is.
#'
#' @inheritParams check_rota
#'
#' @returns A tibble with one row per doctor:
#'   \describe{
#'     \item{doctor_id, grade, pool, wte, opted_out_wtr}{From the `staff`
#'       sheet. `pool` is blank if the sheet has no pools.}
#'     \item{contracted_hours}{Weekly contracted hours: the `staff` sheet's
#'       `contracted_hours`, or the `HOURS_TARGET` rule's value x WTE where
#'       that is blank.}
#'     \item{from, to, weeks_in_post}{Part of the rota period the doctor is
#'       in post.}
#'     \item{hours, leave_hours}{Rostered hours, and approved leave on days
#'       the doctor would otherwise work (weekdays other than bank holidays and
#'       their LTFT day off), counted as a normal day each.}
#'     \item{avg_weekly_hours}{`hours + leave_hours` per week in post. This is
#'       what `AVG_HOURS` limits and what `contracted_hours` is compared with.}
#'     \item{on_calls, nights, weekends}{Counts. A weekend is worked if any
#'       shift falls between 00:00 Saturday and 24:00 Sunday.}
#'     \item{fair_on_calls, fair_nights, fair_weekends}{WTE-weighted fair
#'       share of the pool's total. Nights-exempt doctors have no share of
#'       the nights.}
#'     \item{one column per on-call slot, e.g. `1ST`, and `fair_1ST`}{Only
#'       when the `FAIR_SLOTS` rule is on: how many times each doctor filled
#'       the slot (a 24-hour shift counts towards both slots it fills), and
#'       their fair share among doctors who can work it.}
#'   }
#'
#' @export
#' @examples
#' summarise_doctors(
#'   system.file("extdata", "example_rota_output.xlsx", package = "rotawriter"),
#'   system.file("extdata", "rota_inputs.xlsx", package = "rotawriter")
#' )
summarise_doctors <- function(rota, inputs) {
  inputs <- as_rota_inputs(inputs)
  doctor_summary(shift_instances(as_rota(rota), inputs), inputs)
}

#' Summarise each doctor's rota pattern
#'
#' Counts the patterns the run-length and wellbeing rules look at, so a
#' coordinator can see how the rota feels to work. They are counted whether
#' or not the matching rule is switched on, against the rule's value where
#' it has one.
#'
#' @inheritParams check_rota
#'
#' @returns A tibble with one row per doctor in the staff list, then a row
#'   with `doctor_id` `"All"` holding the totals (and the longest runs):
#'   \describe{
#'     \item{shift_runs, longest_run}{Runs of consecutive days with a shift,
#'       and the longest.}
#'     \item{long_runs}{Runs longer than the `LONG_RUN` value (5 if blank).}
#'     \item{night_runs, single_nights, longest_night_run}{Runs of nights,
#'       those of a single night, and the longest.}
#'     \item{icu_runs, short_icu_runs}{Runs of ICU day shifts, and those
#'       shorter than the `ICU_SHORT_RUN` value (3 if blank), not counting
#'       runs at either end of the rota.}
#'     \item{split_weekends}{Weekends with working time on only one day.}
#'     \item{single_days_off}{Single days off (not leave) between shifts.}
#'   }
#'
#' @export
#' @examples
#' summarise_patterns(
#'   system.file("extdata", "example_rota_output.xlsx", package = "rotawriter"),
#'   system.file("extdata", "rota_inputs.xlsx", package = "rotawriter")
#' )
summarise_patterns <- function(rota, inputs) {
  inputs <- as_rota_inputs(inputs)
  si <- shift_instances(as_rota(rota), inputs)
  value_or <- function(id, default) {
    value <- rule_value(inputs, id)
    if (length(value) == 1 && !is.na(value)) value else default
  }
  long_n <- value_or("LONG_RUN", 5)

  runs <- day_runs(si, TRUE) |>
    summarise(shift_runs = n(), longest_run = max(n), long_runs = sum(n > long_n), .by = doctor_id)
  nights <- day_runs(si, is_night) |>
    summarise(night_runs = n(), single_nights = sum(n == 1), longest_night_run = max(n),
              .by = doctor_id)
  icu <- day_runs(si, !is_night & stringr::str_starts(role, "ICU")) |>
    count(doctor_id, name = "icu_runs") |>
    left_join(count(short_icu_runs(si, inputs, value_or("ICU_SHORT_RUN", 3)), doctor_id,
                    name = "short_icu_runs"), by = "doctor_id")

  doctors <- select(inputs$staff, doctor_id) |>
    left_join(runs, by = "doctor_id") |>
    left_join(nights, by = "doctor_id") |>
    left_join(icu, by = "doctor_id") |>
    left_join(count(split_weekends(si, inputs), doctor_id, name = "split_weekends"), by = "doctor_id") |>
    left_join(count(isolated_days_off(si, inputs), doctor_id, name = "single_days_off"), by = "doctor_id") |>
    mutate(across(-doctor_id, \(x) as.integer(coalesce(x, 0L))))

  longest <- c("longest_run", "longest_night_run")
  total <- doctors |>
    summarise(across(-all_of(c("doctor_id", longest)), sum), across(all_of(longest), max)) |>
    mutate(doctor_id = "All")
  bind_rows(doctors, total)
}

doctor_summary <- function(si, inputs) {
  counts <- si |>
    summarise(hours = sum(hours), on_calls = sum(is_on_call), nights = sum(is_night),
              .by = doctor_id)
  weekends <- count(weekends_worked(si), doctor_id, name = "weekends")

  inputs$staff |>
    mutate(contracted_hours = contracted_hours(inputs), pool = staff_pool(inputs)) |>
    transmute(
      doctor_id, grade, pool, wte, contracted_hours, opted_out_wtr, nights_exempt,
      from = pmax(start_date, inputs$period$start),
      to = pmin(end_date, inputs$period$end),
      weeks_in_post = as.numeric(to - from + 1) / 7
    ) |>
    left_join(counts, by = "doctor_id") |>
    left_join(weekends, by = "doctor_id") |>
    left_join(leave_hours(inputs), by = "doctor_id") |>
    relocate(leave_hours, .after = hours) |>
    mutate(
      across(c(hours, leave_hours, on_calls, nights, weekends), \(x) coalesce(x, 0)),
      avg_weekly_hours = round((hours + leave_hours) / weeks_in_post, 1),
      share = wte * weeks_in_post / sum(wte * weeks_in_post),
      # Nights-exempt doctors take no share of the nights
      night_share = if_else(nights_exempt %in% TRUE, 0, wte * weeks_in_post),
      fair_on_calls = share * sum(on_calls),
      fair_nights = night_share / sum(night_share) * sum(nights),
      fair_weekends = share * sum(weekends),
      across(c(fair_on_calls, fair_nights, fair_weekends), \(x) round(x, 1)),
      .by = pool
    ) |>
    select(-share, -night_share, -nights_exempt) |>
    left_join(slot_summary(si, inputs), by = "doctor_id")
}

# With FAIR_SLOTS on: each doctor's count of each on-call cover slot (named
# after the slot, e.g. `C1`), and their fair share (`fair_C1`), shared within
# the pool among doctors who can work the slot. A 24-hour shift counts towards
# both slots it fills.
slot_summary <- function(si, inputs) {
  if (!rule_on(inputs, "FAIR_SLOTS") || !"FAIR_SLOTS" %in% inputs$rules$rule_id) {
    return(select(inputs$staff, doctor_id))
  }
  slots <- on_call_slots(inputs)
  counts <- rota_slots(si, inputs) |>
    filter(slot %in% slots) |>
    count(doctor_id, slot)
  eligible <- slot_eligible(inputs) |>
    filter(slot %in% slots)

  inputs$staff |>
    transmute(doctor_id, pool = staff_pool(inputs), wte,
              weeks = as.numeric(pmin(end_date, inputs$period$end) -
                                   pmax(start_date, inputs$period$start) + 1) / 7) |>
    inner_join(eligible, by = "doctor_id", relationship = "many-to-many") |>
    left_join(counts, by = c("doctor_id", "slot")) |>
    mutate(n = coalesce(n, 0L),
           fair = round(wte * weeks / sum(wte * weeks) * sum(n), 1), .by = c(pool, slot)) |>
    mutate(slot = factor(slot, levels = slots)) |>
    arrange(slot) |>
    pivot_wider(id_cols = doctor_id, names_from = slot, values_from = c(n, fair),
                names_glue = "{if_else(.value == 'n', '', 'fair_')}{slot}", values_fill = 0) |>
    right_join(select(inputs$staff, doctor_id), by = "doctor_id")
}

# Cover slots whose own shift is on-call
on_call_slots <- function(inputs) {
  slots <- unique(shift_covers(inputs$shifts)$slot)
  slots[slots %in% inputs$shifts$shift_code[inputs$shifts$is_on_call %in% TRUE]]
}

# Doctors who can work each slot at all: a shift filling it is open to them
# by competency and preference (placements and leave aside)
slot_eligible <- function(inputs) {
  can <- staff_competencies(inputs)
  tidyr::crossing(doctor_id = inputs$staff$doctor_id, shift_code = inputs$shifts$shift_code) |>
    left_join(select(inputs$shifts, shift_code, role, is_night), by = "shift_code") |>
    left_join(can, by = c("doctor_id", "role")) |>
    left_join(select(inputs$staff, doctor_id, nights_exempt), by = "doctor_id") |>
    filter(!(rule_on(inputs, "COMPETENCY") & can %in% FALSE),
           !(rule_on(inputs, "NIGHTS_EXEMPT") & is_night & nights_exempt %in% TRUE)) |>
    anti_join(if (rule_on(inputs, "PREFERENCE_ONLY")) preference_only_shifts(inputs) else
                tibble::tibble(doctor_id = character(), shift_code = character()),
              by = c("doctor_id", "shift_code")) |>
    inner_join(shift_covers(inputs$shifts), by = "shift_code", relationship = "many-to-many") |>
    distinct(doctor_id, slot)
}
