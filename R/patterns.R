# Block patterns (the optional `patterns` sheet): the weekdays a slot is
# worked as one block, such as 1st-on Mon-Thu or Fri-Sun, or nights
# Mon/Wed/Fri/Sun. Every pattern lies within one Monday-Sunday week.
#
# Each block (doctor, pattern, whole week) is a binary variable y. For each
# doctor, patterned slot and day, the blocks covering the day can only be
# worked if the slot is:
#   sum(y covering the day) <= sum(x filling the slot)
# Each day a doctor fills a patterned slot costs the PATTERNS weight, and each
# block earns it back for every day it covers, so the penalty is the weight
# times the days not in a block. The solver splits each week into blocks the
# cheapest way, and check_patterns() does the same.

weekday_names <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

weekday_of <- function(date) {
  weekday_names[lubridate::wday(date, week_start = 1)]
}

# One row per pattern day: pattern_id, slot, open_to and offset (0 = Monday)
pattern_days <- function(inputs) {
  inputs$patterns |>
    transmute(pattern_id, slot, open_to = tolower(coalesce(open_to, "all")),
              day = stringr::str_split(days, ";")) |>
    unnest(day) |>
    mutate(offset = match(stringr::str_trim(day), weekday_names) - 1L) |>
    select(-day)
}

# Patterns each doctor may work: those open to all, and those open only by
# preference to doctors who prefer them
doctor_patterns <- function(inputs) {
  preferred <- inputs$preferences |>
    filter(preference == "prefer_pattern") |>
    distinct(doctor_id, pattern_id = value) |>
    mutate(preferred = TRUE)
  pattern_days(inputs) |>
    distinct(pattern_id, slot, open_to) |>
    tidyr::crossing(doctor_id = inputs$staff$doctor_id) |>
    left_join(preferred, by = c("doctor_id", "pattern_id")) |>
    filter(open_to != "preference" | preferred %in% TRUE | !rule_on(inputs, "PREFERENCE_ONLY")) |>
    select(doctor_id, pattern_id, slot)
}

# Cost of each block a doctor works, from their preferences (before the
# PREFERENCES rule weight): avoid_pattern costs its weight per block, and
# prefer_pattern costs its weight per block of any other pattern for the same
# slot
block_costs <- function(inputs) {
  prefs <- mutate(inputs$preferences, weight = coalesce(weight, 1))
  pats <- distinct(pattern_days(inputs), pattern_id, slot)
  avoid <- prefs |>
    filter(preference == "avoid_pattern") |>
    transmute(doctor_id, pattern_id = value, cost = weight)
  prefer <- prefs |>
    filter(preference == "prefer_pattern") |>
    transmute(doctor_id, preferred = value, cost = weight) |>
    inner_join(pats, by = c(preferred = "pattern_id")) |>
    inner_join(pats, by = "slot", relationship = "many-to-many") |>
    filter(pattern_id != preferred) |>
    select(doctor_id, pattern_id, cost)
  bind_rows(avoid, prefer) |>
    summarise(cost = sum(cost), .by = c(doctor_id, pattern_id))
}

# Day index t of the Monday of each whole week in the rota
rota_weeks <- function(inputs) {
  dates <- rota_dates(inputs)
  mondays <- dates[lubridate::wday(dates, week_start = 1) == 1 & dates + 6 <= inputs$period$end]
  as.integer(mondays - inputs$period$start) + 1L
}

patterns_part <- function(x, inputs) {
  weight <- soft_weight(inputs, "PATTERNS")
  if (weight == 0 || nrow(inputs$patterns) == 0) {
    return(NULL)
  }
  days <- pattern_days(inputs)
  fill <- x |>
    select(doctor_id, t, shift_code, j) |>
    inner_join(shift_covers(inputs$shifts), by = "shift_code", relationship = "many-to-many") |>
    filter(slot %in% days$slot) |>
    mutate(con = paste("PATTERNS", doctor_id, slot, t))

  # Blocks only where the doctor can fill the slot on every day of the block
  block_days <- doctor_patterns(inputs) |>
    inner_join(select(days, pattern_id, offset), by = "pattern_id", relationship = "many-to-many") |>
    tidyr::crossing(week = rota_weeks(inputs)) |>
    mutate(t = week + offset, aux = paste("PATTERNS", doctor_id, pattern_id, week)) |>
    semi_join(fill, by = c("doctor_id", "slot", "t")) |>
    left_join(count(days, pattern_id, name = "len"), by = "pattern_id") |>
    filter(n() == len, .by = aux) |>
    mutate(con = paste("PATTERNS", doctor_id, slot, t))
  if (nrow(block_days) == 0) {
    return(list(costs = transmute(fill, rule = "PATTERNS", j, coef = weight)))
  }
  blocks <- block_days |>
    distinct(aux, doctor_id, pattern_id, week, len) |>
    left_join(block_costs(inputs), by = c("doctor_id", "pattern_id"))
  preference_weight <- soft_weight(inputs, "PREFERENCES")

  list(
    vars = aux_vars(blocks$aux, type = "I") |>
      mutate(kind = "block", doctor_id = blocks$doctor_id, t = blocks$week),
    terms = bind_rows(
      transmute(semi_join(fill, block_days, by = "con"), con, j, coef = 1),
      transmute(block_days, con, aux, coef = -1)
    ) |>
      mutate(rule = "PATTERNS", lo = 0, hi = Inf),
    costs = bind_rows(
      transmute(fill, rule = "PATTERNS", j, coef = weight),
      transmute(blocks, rule = "PATTERNS", aux, coef = -weight * len),
      blocks |>
        filter(cost > 0, preference_weight > 0) |>
        transmute(rule = "PREFERENCES", aux, coef = cost * preference_weight)
    )
  )
}

# Each doctor's week of each patterned slot split into blocks the cheapest
# way, as the solver does: PATTERNS for the days left out of a block, and
# PREFERENCES for blocks their preferences cost
check_patterns <- function(si, inputs) {
  if (nrow(inputs$patterns) == 0) {
    return(NULL)
  }
  day_cost <- soft_weight(inputs, "PATTERNS")
  preference_weight <- soft_weight(inputs, "PREFERENCES")
  days <- pattern_days(inputs)
  mondays <- inputs$period$start + rota_weeks(inputs) - 1

  allowed <- doctor_patterns(inputs) |>
    inner_join(summarise(days, mask = sum(2^offset), .by = pattern_id), by = "pattern_id") |>
    left_join(block_costs(inputs), by = c("doctor_id", "pattern_id")) |>
    mutate(cost = coalesce(cost, 0))
  weeks <- rota_slots(si, inputs) |>
    filter(slot %in% days$slot) |>
    distinct(doctor_id, slot, date) |>
    mutate(monday = date - lubridate::wday(date, week_start = 1) + 1) |>
    summarise(mask = sum(2^as.numeric(date - monday)), .by = c(doctor_id, slot, monday))

  pmap(weeks, \(doctor_id, slot, monday, mask) {
    candidates <- if (monday %in% mondays) {
      allowed[allowed$doctor_id == doctor_id & allowed$slot == slot, ]
    } else {
      allowed[0, ]
    }
    best <- split_into_blocks(mask, as.integer(candidates$mask), candidates$cost * preference_weight,
                              day_cost)
    chosen <- candidates[best$blocks, ]
    bind_rows(
      if (best$left > 0) {
        violations("PATTERNS", doctor_id, monday, monday + 6, weight = best$left,
                   detail = sprintf("%d %s day%s outside a block pattern", best$left, slot,
                                    if_else(best$left == 1, "", "s")))
      },
      chosen |>
        filter(cost > 0) |>
        with(violations("PREFERENCES", doctor_id, monday, monday + 6, weight = cost,
                        detail = paste(pattern_id, "block (against preferences)")))
    )
  }) |>
    list_rbind()
}

# The cheapest split of the weekdays in `mask` (a bitmask, bit 0 = Monday)
# into blocks (bitmasks `block_masks`, each costing `block_costs`) and days
# left out (each costing `day_cost`). Returns the indices of the blocks
# chosen and the number of days left out.
split_into_blocks <- function(mask, block_masks, block_costs, day_cost) {
  memo <- list()
  best <- function(m) {
    if (m == 0) {
      return(list(cost = 0, blocks = integer(), left = 0))
    }
    key <- as.character(m)
    if (!is.null(memo[[key]])) {
      return(memo[[key]])
    }
    # The lowest day is either left out or starts a block
    low <- bitwAnd(m, -m)
    rest <- best(m - low)
    choice <- list(cost = rest$cost + day_cost, blocks = rest$blocks, left = rest$left + 1)
    for (b in seq_along(block_masks)) {
      bm <- block_masks[b]
      if (bitwAnd(bm, low) != 0 && bitwAnd(bm, m) == bm) {
        rest <- best(m - bm)
        if (rest$cost + block_costs[b] < choice$cost) {
          choice <- list(cost = rest$cost + block_costs[b], blocks = c(b, rest$blocks), left = rest$left)
        }
      }
    }
    memo[[key]] <<- choice
    choice
  }
  best(as.integer(mask))
}

# Preference-only patterns worked in full by doctors who have not asked for
# them, e.g. 1st on Mon-Thu then Fri-Sun in one week for someone who has not
# asked for 7-day weeks (rule PATTERN_UNPREFERRED, per week):
#   unpreferred[d, pattern, week] >= sum(x filling the slot on its days) - (len - 1)
unpreferred_candidates <- function(inputs) {
  preferred <- inputs$preferences |>
    filter(preference == "prefer_pattern") |>
    distinct(doctor_id, pattern_id = value)
  pattern_days(inputs) |>
    filter(open_to == "preference") |>
    distinct(pattern_id, slot) |>
    tidyr::crossing(doctor_id = inputs$staff$doctor_id) |>
    anti_join(preferred, by = c("doctor_id", "pattern_id"))
}

unpreferred_part <- function(x, inputs) {
  weight <- soft_weight(inputs, "PATTERN_UNPREFERRED")
  candidates <- unpreferred_candidates(inputs)
  if (weight == 0 || nrow(candidates) == 0) {
    return(NULL)
  }
  days <- pattern_days(inputs)
  fill <- x |>
    select(doctor_id, t, shift_code, j) |>
    inner_join(shift_covers(inputs$shifts), by = "shift_code", relationship = "many-to-many") |>
    filter(slot %in% candidates$slot)
  weeks <- candidates |>
    inner_join(select(days, pattern_id, offset), by = "pattern_id", relationship = "many-to-many") |>
    tidyr::crossing(week = rota_weeks(inputs)) |>
    mutate(t = week + offset, aux = paste("PATTERN_UNPREFERRED", doctor_id, pattern_id, week)) |>
    left_join(count(days, pattern_id, name = "len"), by = "pattern_id") |>
    # Only weeks the doctor could fill the slot every day of the pattern
    semi_join(fill, by = c("doctor_id", "slot", "t")) |>
    filter(n() == len, .by = aux)
  if (nrow(weeks) == 0) {
    return(NULL)
  }
  found <- distinct(weeks, aux, len)

  list(
    vars = aux_vars(found$aux),
    terms = bind_rows(
      weeks |>
        inner_join(fill, by = c("doctor_id", "slot", "t"), relationship = "many-to-many") |>
        transmute(con = aux, j, coef = 1),
      transmute(found, con = aux, aux, coef = -1)
    ) |>
      mutate(rule = "PATTERN_UNPREFERRED", lo = -Inf, hi = found$len[match(con, found$aux)] - 1),
    costs = transmute(found, rule = "PATTERN_UNPREFERRED", aux, coef = weight)
  )
}

check_unpreferred <- function(si, inputs) {
  candidates <- unpreferred_candidates(inputs)
  if (nrow(candidates) == 0) {
    return(NULL)
  }
  mondays <- inputs$period$start + rota_weeks(inputs) - 1
  masks <- summarise(pattern_days(inputs), pattern_mask = sum(2^offset), .by = pattern_id)
  rota_slots(si, inputs) |>
    filter(slot %in% candidates$slot) |>
    distinct(doctor_id, slot, date) |>
    mutate(monday = date - lubridate::wday(date, week_start = 1) + 1) |>
    filter(monday %in% mondays) |>
    summarise(mask = sum(2^as.numeric(date - monday)), .by = c(doctor_id, slot, monday)) |>
    inner_join(candidates, by = c("doctor_id", "slot"), relationship = "many-to-many") |>
    inner_join(masks, by = "pattern_id") |>
    filter(bitwAnd(as.integer(mask), as.integer(pattern_mask)) == pattern_mask) |>
    with(violations("PATTERN_UNPREFERRED", doctor_id, monday, monday + 6,
                    detail = paste("Works the", pattern_id, "pattern without preferring it")))
}

# Rest around big blocks (the optional `block_rest` sheet), such as 1st on
# all seven days of a week or a night block worked as 24-hour shifts: a
# doctor who works every day of the row's pattern with its shifts should
# have `rest_before_h` hours off before the block's first shift starts and
# `rest_after_h` after its last ends. Each shift cutting into that rest costs
# the BLOCK_REST weight (rule per shift). For a block's days B and a shift k
# in its rest:
#   cut[d, row, week, k] >= sum(x on B) + x[k] - len
# The rest is timed from the shift that starts (ends) the block, so the first
# (last) day has one such row per shift it could be.

# One row per block_rest row, pattern day and shift that may work the day:
# row, pattern_id, offset (0 = Monday), shift_code, len (days in the
# pattern), first, last, before and after (hours)
block_rest_days <- function(inputs) {
  if (nrow(inputs$block_rest) == 0) {
    return(NULL)
  }
  rows <- inputs$block_rest |>
    transmute(row = row_number(), pattern_id, shifts = as.character(shifts),
              before = coalesce(as.numeric(rest_before_h), 0),
              after = coalesce(as.numeric(rest_after_h), 0))
  rows |>
    inner_join(pattern_days(inputs), by = "pattern_id", relationship = "many-to-many") |>
    inner_join(shift_covers(inputs$shifts), by = "slot", relationship = "many-to-many") |>
    # A blank `shifts` means any shift that fills the pattern's slot
    filter(is.na(shifts) | map2_lgl(shift_code, shifts, \(code, codes) {
      code %in% stringr::str_trim(stringr::str_split_1(codes, ";"))
    })) |>
    mutate(len = n_distinct(offset), first = offset == min(offset), last = offset == max(offset),
           .by = row) |>
    select(row, pattern_id, offset, shift_code, len, first, last, before, after)
}

block_rest_part <- function(x, inputs) {
  weight <- soft_weight(inputs, "BLOCK_REST")
  days <- block_rest_days(inputs)
  if (weight == 0 || is.null(days)) {
    return(NULL)
  }
  # Hours from the rota start, as in the checker's clock times
  xs <- transmute(x, doctor_id, t, shift_code, j, from = 24 * t + start_h, to = 24 * t + end_h)
  longest <- max(x$end_h - x$start_h, 0)

  # Weeks each doctor could work a whole block
  block_x <- days |>
    tidyr::crossing(week = rota_weeks(inputs)) |>
    mutate(t = week + offset) |>
    inner_join(xs, by = c("t", "shift_code")) |>
    mutate(block = paste(row, doctor_id, week)) |>
    filter(n_distinct(t) == len, .by = block)
  edges <- bind_rows(
    block_x |>
      filter(first, before > 0) |>
      mutate(rest_from = from - before, rest_to = from,
             t_lo = t - ceiling((before + longest) / 24) - 1, t_hi = t - 1),
    block_x |>
      filter(last, after > 0) |>
      mutate(rest_from = to, rest_to = to + after, t_lo = t + 1, t_hi = t + ceiling(after / 24) + 1)
  ) |>
    select(block, doctor_id, len, edge_t = t, edge_j = j, rest_from, rest_to, t_lo, t_hi)
  cuts <- edges |>
    inner_join(rename(xs, k_from = from, k_to = to),
               join_by(doctor_id, between(y$t, x$t_lo, x$t_hi))) |>
    filter(k_from < rest_to, k_to > rest_from) |>
    mutate(aux = paste("BLOCK_REST", block, j), con = paste(aux, edge_j))
  if (nrow(cuts) == 0) {
    return(NULL)
  }
  others <- cuts |>
    distinct(con, block, edge_t, len) |>
    inner_join(select(block_x, block, t, j), by = "block", relationship = "many-to-many") |>
    filter(t != edge_t)
  found <- distinct(cuts, aux)

  list(
    vars = aux_vars(found$aux),
    terms = bind_rows(
      transmute(others, con, j, coef = 1, hi = len),
      transmute(cuts, con, j = edge_j, coef = 1, hi = len),
      transmute(cuts, con, j, coef = 1, hi = len),
      transmute(cuts, con, aux, coef = -1, hi = len)
    ) |>
      mutate(rule = "BLOCK_REST", lo = -Inf),
    costs = transmute(found, rule = "BLOCK_REST", aux, coef = weight)
  )
}

# Each shift a doctor works in the rest before or after a whole block, in
# clock time
check_block_rest <- function(si, inputs) {
  days <- block_rest_days(inputs)
  if (is.null(days) || !"BLOCK_REST" %in% inputs$rules$rule_id) {
    return(NULL)
  }
  mondays <- inputs$period$start + rota_weeks(inputs) - 1
  worked <- select(si, doctor_id, date, code, start_dt, end_dt)

  blocks <- worked |>
    mutate(monday = date - lubridate::wday(date, week_start = 1) + 1,
           offset = as.integer(date - monday)) |>
    filter(monday %in% mondays) |>
    inner_join(days, by = c("offset", code = "shift_code"), relationship = "many-to-many") |>
    filter(n_distinct(date) == len, .by = c(row, doctor_id, monday))
  if (nrow(blocks) == 0) {
    return(NULL)
  }
  blocks <- blocks |>
    summarise(first = min(start_dt), last = max(end_dt),
              .by = c(row, pattern_id, doctor_id, monday, before, after))

  blocks |>
    inner_join(worked, by = "doctor_id", relationship = "many-to-many") |>
    mutate(side = case_when(
      start_dt < first & end_dt > first - before * 3600 ~ "before",
      end_dt > last & start_dt < last + after * 3600 ~ "after"
    )) |>
    filter(!is.na(side)) |>
    with(violations(
      "BLOCK_REST", doctor_id, date,
      detail = sprintf("%s within %g h %s a %s block (week of %s)", code,
                       if_else(side == "before", before, after), side, pattern_id,
                       format(monday, "%d %b"))
    ))
}

# Patterns worked back to back across two weeks (the optional
# `pattern_sequences` sheet), such as 1st on Fri-Sun then Mon-Thu: 7 days
# running across the weekend. Each costs the row's weight times the
# PATTERN_SEQUENCE weight:
#   seq[d, row, week] >= sum(x on from_pattern's days in the week and
#                            to_pattern's days in the next) - (len - 1)

# One row per pattern_sequences row and day of the pair: row, from_pattern,
# to_pattern, weight, slot, offset (0 = the first week's Monday) and len
pattern_sequence_days <- function(inputs) {
  if (nrow(inputs$pattern_sequences) == 0) {
    return(NULL)
  }
  days <- select(pattern_days(inputs), pattern_id, slot, offset)
  rows <- inputs$pattern_sequences |>
    transmute(row = row_number(), from_pattern, to_pattern, weight = coalesce(as.numeric(weight), 1))
  bind_rows(
    inner_join(rows, days, by = c(from_pattern = "pattern_id"), relationship = "many-to-many"),
    inner_join(rows, days, by = c(to_pattern = "pattern_id"), relationship = "many-to-many") |>
      mutate(offset = offset + 7L)
  ) |>
    distinct() |>
    mutate(len = n(), .by = row)
}

# Mondays (as day indices) of whole weeks followed by another whole week
paired_weeks <- function(inputs) {
  weeks <- rota_weeks(inputs)
  weeks[(weeks + 7L) %in% weeks]
}

pattern_sequence_part <- function(x, inputs) {
  weight <- soft_weight(inputs, "PATTERN_SEQUENCE")
  days <- pattern_sequence_days(inputs)
  if (weight == 0 || is.null(days)) {
    return(NULL)
  }
  fill <- x |>
    select(doctor_id, t, shift_code, j) |>
    inner_join(shift_covers(inputs$shifts), by = "shift_code", relationship = "many-to-many") |>
    filter(slot %in% days$slot)
  pairs <- days |>
    tidyr::crossing(week = paired_weeks(inputs), doctor_id = inputs$staff$doctor_id) |>
    mutate(t = week + offset, aux = paste("PATTERN_SEQUENCE", doctor_id, row, week)) |>
    # Only pairs of weeks the doctor could fill every day of
    semi_join(fill, by = c("doctor_id", "slot", "t")) |>
    filter(n() == len, .by = aux)
  if (nrow(pairs) == 0) {
    return(NULL)
  }
  found <- distinct(pairs, aux, len, weight)

  list(
    vars = aux_vars(found$aux),
    terms = bind_rows(
      pairs |>
        inner_join(fill, by = c("doctor_id", "slot", "t"), relationship = "many-to-many") |>
        transmute(con = aux, j, coef = 1, hi = len - 1),
      transmute(found, con = aux, aux, coef = -1, hi = len - 1)
    ) |>
      mutate(rule = "PATTERN_SEQUENCE", lo = -Inf),
    costs = transmute(found, rule = "PATTERN_SEQUENCE", aux, coef = weight * .env$weight)
  )
}

check_pattern_sequences <- function(si, inputs) {
  days <- pattern_sequence_days(inputs)
  if (is.null(days) || !"PATTERN_SEQUENCE" %in% inputs$rules$rule_id) {
    return(NULL)
  }
  worked <- distinct(rota_slots(si, inputs), doctor_id, slot, date)
  found <- days |>
    tidyr::crossing(monday = inputs$period$start + paired_weeks(inputs) - 1) |>
    mutate(date = monday + offset) |>
    inner_join(worked, by = c("slot", "date")) |>
    filter(n() == len, .by = c(row, doctor_id, monday))
  if (nrow(found) == 0) {
    return(NULL)
  }
  found |>
    summarise(first = min(date), last = max(date),
              .by = c(row, from_pattern, to_pattern, weight, doctor_id, monday)) |>
    with(violations("PATTERN_SEQUENCE", doctor_id, first, last, weight = weight,
                    detail = paste(from_pattern, "then", to_pattern, "the next week")))
}
