# Test inputs without leave or requests, so each test controls them
test_inputs <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      inputs <- read_rota_inputs(system.file("extdata", "rota_inputs.xlsx", package = "rotawriter"))
      inputs$leave <- inputs$leave[0, ]
      inputs$requests <- inputs$requests[0, ]
      cache <<- inputs
    }
    cache
  }
})

rota_of <- function(doctor_id, date, code) {
  tibble::tibble(doctor_id, date = as.Date(date), code)
}

# Violations other than COVER, which always fires on a partial rota
hits <- function(rota, inputs = test_inputs()) {
  v <- check_rota(rota, inputs)
  v[v$rule_id != "COVER", ]
}

days_from <- function(start, n) as.Date(start) + seq_len(n) - 1

# The first two weeks of the rota
two_weeks <- function(inputs = read_rota_inputs(system.file("extdata", "rota_inputs.xlsx",
                                                            package = "rotawriter"))) {
  inputs$period$end <- inputs$period$start + 13
  inputs
}

# Each soft rule's penalty for a fixed rota, as the model scores it: every
# shift variable fixed, helper variables solved for their cheapest values
model_penalties <- function(rota, inputs) {
  model <- build_rota_model(inputs)
  on <- paste(model$vars$doctor_id, model$vars$date, model$vars$shift_code) %in%
    paste(rota$doctor_id, as.Date(rota$date), rota$code)
  stopifnot(sum(on & model$vars$kind == "x") == nrow(rota))
  result <- polish(highs_solver(model), model, as.numeric(on & model$vars$kind == "x"))
  dplyr::transmute(model$costs, rule, penalty = coef * result$primal_solution[j]) |>
    dplyr::bind_rows(dplyr::rename(model$offset, penalty = value)) |>
    dplyr::summarise(penalty = round(sum(penalty), 6), .by = rule) |>
    dplyr::filter(!startsWith(rule, "FAIR_"), rule != "COVER", penalty != 0) |>
    dplyr::arrange(rule)
}

# The ICU consultant inputs, without leave or requests, cut to `weeks` weeks.
# WEEKEND_MAX (1 in 3) is off: over two weeks it allows nobody a weekend.
consultant_inputs <- function(weeks = 2) {
  inputs <- read_rota_inputs(system.file("extdata", "icu_consultant_inputs.xlsx", package = "rotawriter"))
  inputs$leave <- inputs$leave[0, ]
  inputs$requests <- inputs$requests[0, ]
  inputs$period$end <- inputs$period$start + 7 * weeks - 1
  inputs$rules$enabled[inputs$rules$rule_id == "WEEKEND_MAX"] <- FALSE
  inputs
}
