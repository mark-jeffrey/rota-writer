# Write a rota, check it, and save everything the coordinator needs.
#
#   Rscript scripts/solve_rota.R <inputs.xlsx> <output_folder> [time_limit_s]
#
# Run from the project root. Writes to the output folder:
#   rota.xlsx      the rota (grid, by_shift, doctors, key and long sheets)
#   report.xlsx    compliance and fairness report from the independent checker
#   calendars/     one .ics calendar per doctor
# Exits with status 1 if no rota was found or it breaks a hard rule
# (including slots left unfilled), and 2 if the inputs fail validation.

pkgload::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript scripts/solve_rota.R <inputs.xlsx> <output_folder> [time_limit_s]",
       call. = FALSE)
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

out <- args[2]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
violations <- check_rota(solution$rota, inputs)

write_rota(solution$rota, inputs, file.path(out, "rota.xlsx"))
write_check_report(violations, summarise_doctors(solution$rota, inputs), file.path(out, "report.xlsx"))
calendars <- write_calendars(solution$rota, inputs, file.path(out, "calendars"))
message(sprintf("Wrote rota.xlsx, report.xlsx and %d calendars to %s", length(calendars), out))

n_hard <- sum(violations$hard_soft == "hard")
message(sprintf("Check: %d hard violations, %d soft (total penalty %g)",
                n_hard, sum(violations$hard_soft == "soft"),
                sum(violations$penalty, na.rm = TRUE)))
print(dplyr::count(violations, hard_soft, rule_id), n = Inf)

quit(status = as.integer(n_hard > 0))
