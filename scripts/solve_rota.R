# Write a rota that meets the hard rules, then check it.
#
#   Rscript scripts/solve_rota.R <inputs.xlsx> <rota.xlsx> [time_limit_s]
#
# Run from the project root. Writes the rota workbook (grid and long sheets)
# and prints the checker's summary. Exits with status 1 if no rota was found
# or it breaks a hard rule (including slots left unfilled), and 2 if the
# inputs fail validation.

pkgload::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript scripts/solve_rota.R <inputs.xlsx> <rota.xlsx> [time_limit_s]", call. = FALSE)
}

inputs <- read_rota_inputs(args[1])
problems <- validate_inputs(inputs)
if (nrow(problems) > 0) {
  message("Input problems:")
  print(problems, n = Inf)
  quit(status = 2)
}

time_limit <- if (length(args) >= 3) as.numeric(args[3])
solution <- solve_rota(inputs, time_limit = time_limit)
print(solution)
if (is.null(solution$rota)) {
  quit(status = 1)
}

write_rota(solution$rota, inputs, args[2])
message("Rota written to ", args[2])

violations <- check_rota(solution$rota, inputs)
n_hard <- sum(violations$hard_soft == "hard")
message(sprintf("Check: %d hard violations, %d soft (total penalty %g)",
                n_hard, sum(violations$hard_soft == "soft"),
                sum(violations$penalty, na.rm = TRUE)))
print(dplyr::count(violations, hard_soft, rule_id), n = Inf)

quit(status = as.integer(n_hard > 0))
