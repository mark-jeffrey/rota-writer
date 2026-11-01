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
