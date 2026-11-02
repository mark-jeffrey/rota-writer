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
