# Check a rota against the contract and local rules.
#
#   Rscript scripts/check_rota.R <inputs.xlsx> <rota.xlsx> [report.xlsx]
#
# Run from the project root. The rota workbook needs a "long" sheet with
# doctor_id, date, code. Exits with status 1 if any hard rule is broken.

pkgload::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript scripts/check_rota.R <inputs.xlsx> <rota.xlsx> [report.xlsx]", call. = FALSE)
}

inputs <- read_rota_inputs(args[1])
problems <- validate_inputs(inputs)
if (nrow(problems) > 0) {
  message("Input problems:")
  print(problems, n = Inf)
  quit(status = 2)
}

rota <- read_rota(args[2])
violations <- check_rota(rota, inputs)
doctors <- summarise_doctors(rota, inputs)

summary <- dplyr::count(violations, hard_soft, rule_id) |>
  dplyr::arrange(dplyr::desc(hard_soft == "hard"), dplyr::desc(n))
n_hard <- sum(violations$hard_soft == "hard")

message(sprintf("%d hard violations, %d soft (total penalty %g)",
                n_hard, sum(violations$hard_soft == "soft"),
                sum(violations$penalty, na.rm = TRUE)))
print(summary, n = Inf)

if (length(args) >= 3) {
  write_check_report(violations, doctors, args[3], patterns = summarise_patterns(rota, inputs))
  message("Report written to ", args[3])
}

quit(status = as.integer(n_hard > 0))
