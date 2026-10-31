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

  checks <- list(
    check_cover, check_in_post, check_competency, check_placement,
    check_nights_exempt, check_ltft_day, check_leave, check_max_shift,
    check_min_rest, check_max_7day, check_avg_hours, check_consecutive,
    check_nights, check_weekends, check_requests, check_skill_mix
  )

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
    select(rule_id, hard_soft, rule_weight = weight, enabled)

  v |>
    left_join(rules, by = "rule_id") |>
    filter(is.na(enabled) | enabled) |>
    mutate(
      hard_soft = coalesce(hard_soft, "hard"),
      penalty = if_else(hard_soft == "soft", coalesce(weight, 1) * coalesce(rule_weight, 1), NA_real_)
    ) |>
    arrange(desc(hard_soft == "hard"), rule_id, start_date, doctor_id) |>
    select(rule_id, hard_soft, doctor_id, start_date, end_date, detail, penalty)
}

# Department-level rules -------------------------------------------------

expected_cover <- function(inputs) {
  tidyr::crossing(date = rota_dates(inputs), shift_code = inputs$shifts$shift_code) |>
    mutate(day_type = day_type(date, inputs)) |>
    left_join(inputs$cover, by = c("shift_code", "day_type")) |>
    mutate(across(c(n_min, n_max), \(x) coalesce(x, 0)))
}

check_cover <- function(si, inputs) {
  actual <- count(si, date, shift_code = code)

  expected_cover(inputs) |>
    left_join(actual, by = c("date", "shift_code")) |>
    mutate(n = coalesce(n, 0L)) |>
    filter(n < n_min | n > n_max) |>
    with(violations(
      "COVER",
      start_date = date,
      detail = sprintf("%s: %d rostered, needs %s", shift_code, n,
                       if_else(n_min == n_max, as.character(n_min), paste0(n_min, "-", n_max)))
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

check_competency <- function(si, inputs) {
  can <- inputs$staff |>
    select(doctor_id, starts_with("can_")) |>
    pivot_longer(-doctor_id, names_to = "role", names_prefix = "can_", values_to = "can")

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
    unnest(date) |>
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

check_avg_hours <- function(si, inputs) {
  limit <- rule_value(inputs, "AVG_HOURS")
  limit_optout <- rule_value(inputs, "AVG_HOURS_OPTOUT")

  doctor_summary(si, inputs) |>
    mutate(limit = if_else(opted_out_wtr, limit_optout, limit) * wte) |>
    filter(avg_weekly_hours > limit + 1e-9) |>
    with(violations(
      "AVG_HOURS", doctor_id, from, to,
      detail = sprintf("Average %.1f h/week over %.1f weeks (max %.1f at WTE %g)",
                       avg_weekly_hours, weeks_in_post, limit, wte)
    ))
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

  imap(run_rules, \(runs, id) {
    max_n <- rule_value(inputs, id)
    what <- c(MAX_CONSEC_SHIFTS = "shifts", MAX_CONSEC_LONG = "long shifts",
              MAX_CONSEC_LATE = "long evening shifts")[[id]]
    bind_rows(
      runs |>
        filter(n > max_n) |>
        with(violations(id, doctor_id, first, last,
                        detail = sprintf("%d consecutive %s (max %g)", n, what, max_n))),
      runs |>
        filter(n == max_n, rest_after < rest) |>
        with(violations("REST_AFTER_RUN", doctor_id, first, last,
                        detail = sprintf("%.1f h rest after %d consecutive %s (min %g)",
                                         rest_after, n, what, rest)))
    )
  }) |>
    list_rbind()
}

check_nights <- function(si, inputs) {
  max_n <- rule_value(inputs, "MAX_CONSEC_NIGHTS")
  rest <- rule_value(inputs, "REST_AFTER_NIGHTS")
  runs <- day_runs(si, is_night)

  bind_rows(
    runs |>
      filter(n > max_n) |>
      with(violations("MAX_CONSEC_NIGHTS", doctor_id, first, last,
                      detail = sprintf("%d consecutive nights (max %g)", n, max_n))),
    runs |>
      filter(rest_after < rest) |>
      with(violations("REST_AFTER_NIGHTS", doctor_id, first, last,
                      detail = sprintf("%.1f h rest after nights (min %g)", rest_after, rest))),
    runs |>
      filter(n == 1) |>
      with(violations("SINGLE_NIGHT", doctor_id, first, last, detail = "Isolated single night"))
  )
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
    freq |>
      filter(worked > total / hard_n) |>
      with(violations("WEEKEND_MAX", doctor_id, from, to,
                      detail = paste0(detail, " (more than 1 in ", hard_n, ")"))),
    freq |>
      mutate(excess = worked - floor(total / target_n)) |>
      filter(excess > 0) |>
      with(violations("WEEKEND_TARGET", doctor_id, from, to,
                      detail = paste0(detail, " (", excess, " more than 1 in ", target_n, ")"),
                      weight = excess))
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
    filter(case_match(request,
      "no_oncall"     ~ on_call,
      "no_night"      ~ night,
      "no_weekend"    ~ weekend,
      "prefer_oncall" ~ !on_call,
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
#' `wte * weeks_in_post`. Use it to judge how even a rota is.
#'
#' @inheritParams check_rota
#'
#' @returns A tibble with one row per doctor:
#'   \describe{
#'     \item{doctor_id, grade, wte, opted_out_wtr}{From the `staff` sheet.}
#'     \item{from, to, weeks_in_post}{Part of the rota period the doctor is
#'       in post.}
#'     \item{hours, avg_weekly_hours}{Rostered hours, in total and per week
#'       in post (leave counts as zero).}
#'     \item{on_calls, nights, weekends}{Counts. A weekend is worked if any
#'       shift falls between 00:00 Saturday and 24:00 Sunday.}
#'     \item{fair_on_calls, fair_nights, fair_weekends}{WTE-weighted fair
#'       share of the department total.}
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

doctor_summary <- function(si, inputs) {
  counts <- si |>
    summarise(hours = sum(hours), on_calls = sum(is_on_call), nights = sum(is_night),
              .by = doctor_id)
  weekends <- count(weekends_worked(si), doctor_id, name = "weekends")

  inputs$staff |>
    transmute(
      doctor_id, grade, wte, opted_out_wtr,
      from = pmax(start_date, inputs$period$start),
      to = pmin(end_date, inputs$period$end),
      weeks_in_post = as.numeric(to - from + 1) / 7
    ) |>
    left_join(counts, by = "doctor_id") |>
    left_join(weekends, by = "doctor_id") |>
    mutate(
      across(c(hours, on_calls, nights, weekends), \(x) coalesce(x, 0)),
      avg_weekly_hours = hours / weeks_in_post,
      share = wte * weeks_in_post / sum(wte * weeks_in_post),
      fair_on_calls = share * sum(on_calls),
      fair_nights = share * sum(nights),
      fair_weekends = share * sum(weekends),
      across(c(fair_on_calls, fair_nights, fair_weekends), \(x) round(x, 1)),
      avg_weekly_hours = round(avg_weekly_hours, 1)
    ) |>
    select(-share)
}
