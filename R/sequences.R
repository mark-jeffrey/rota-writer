# Shift sequences (the optional `sequences` sheet): `to_shift` worked
# `days_after` days after `from_shift`, e.g. no 1st-on day before a night.
# Rows with a blank weight are hard (rule SEQUENCE):
#   x[d, t, from] + x[d, t + k, to] <= 1
# Rows with a weight are soft (rule SEQUENCE_SOFT), each time costing the
# row's weight times the rule's:
#   seq[d, t, row] >= x[d, t, from] + x[d, t + k, to] - 1

# Each pair of shift variables a sequence row covers: `row`, `hard`,
# `penalty`, `con` and the two variables `j_from`, `j_to`
sequence_pairs <- function(x, inputs) {
  if (nrow(inputs$sequences) == 0) {
    return(NULL)
  }
  rows <- inputs$sequences |>
    transmute(row = row_number(), shift_code = from_shift, to_shift, k = as.integer(days_after),
              hard = is.na(weight), penalty = weight)
  x |>
    select(doctor_id, t, shift_code, j_from = j) |>
    inner_join(rows, by = "shift_code", relationship = "many-to-many") |>
    mutate(t_to = t + k) |>
    inner_join(select(x, doctor_id, t_to = t, to_shift = shift_code, j_to = j),
               by = c("doctor_id", "t_to", "to_shift")) |>
    mutate(con = paste("SEQUENCE", doctor_id, t, row))
}

sequence_part <- function(x, inputs) {
  if (!rule_on(inputs, "SEQUENCE")) {
    return(NULL)
  }
  pairs <- sequence_pairs(x, inputs)
  if (is.null(pairs) || !any(pairs$hard)) {
    return(NULL)
  }
  pairs <- filter(pairs, hard)
  list(terms = bind_rows(
    transmute(pairs, con, j = j_from, coef = 1),
    transmute(pairs, con, j = j_to, coef = 1)
  ) |>
    constraint("SEQUENCE", hi = 1))
}

sequence_soft_part <- function(x, inputs) {
  weight <- soft_weight(inputs, "SEQUENCE_SOFT")
  pairs <- sequence_pairs(x, inputs)
  if (weight == 0 || is.null(pairs) || all(pairs$hard)) {
    return(NULL)
  }
  pairs <- filter(pairs, !hard)
  list(
    vars = aux_vars(pairs$con),
    terms = bind_rows(
      transmute(pairs, con, j = j_from, coef = 1),
      transmute(pairs, con, j = j_to, coef = 1),
      transmute(pairs, con, aux = con, coef = -1)
    ) |>
      mutate(rule = "SEQUENCE_SOFT", lo = -Inf, hi = 1),
    costs = transmute(pairs, rule = "SEQUENCE_SOFT", aux = con, coef = penalty * weight)
  )
}

check_sequences <- function(si, inputs) {
  if (nrow(inputs$sequences) == 0) {
    return(NULL)
  }
  rows <- inputs$sequences |>
    transmute(code = from_shift, to_shift, k = as.integer(days_after), weight)
  si |>
    select(doctor_id, date, code) |>
    inner_join(rows, by = "code", relationship = "many-to-many") |>
    mutate(to_date = date + k) |>
    inner_join(select(si, doctor_id, to_date = date, to_shift = code),
               by = c("doctor_id", "to_date", "to_shift")) |>
    with(violations(
      if_else(is.na(weight), "SEQUENCE", "SEQUENCE_SOFT"), doctor_id, date, to_date,
      detail = sprintf("%s then %s %d day%s later", code, to_shift, k, if_else(k == 1, "", "s")),
      weight = weight
    ))
}
