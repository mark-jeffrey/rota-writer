# Install the R packages rota-writer needs, then check that it works.
#
#   Rscript scripts/setup.R [--no-app] [--templates] [--no-check]
#
# Run from the project root. Installs only the packages that are missing:
#   the solver, checker and command-line scripts always;
#   the app (shiny and friends) unless --no-app;
#   tidyverse, for the data-raw/ scripts that write input templates, with --templates.
# Then it loads the package, validates the example inputs and solves them
# for 60 seconds (skip this with --no-check). Exits with status 1 if
# anything fails.

args <- commandArgs(trailingOnly = TRUE)

say <- function(...) message(paste0(...))
fail <- function(...) {
  say("\nSETUP FAILED: ", ...)
  quit(status = 1)
}

if (!file.exists("DESCRIPTION") || !any(grepl("^Package: rotawriter", readLines("DESCRIPTION")))) {
  fail("run this from the rota-writer folder (the one with DESCRIPTION in it).")
}
if (getRversion() < "4.3.0") {
  fail("R ", getRversion(), " is too old. Install R 4.3 or newer from https://cran.r-project.org.")
}

packages <- c("dplyr", "highs", "lubridate", "Matrix", "openxlsx2", "purrr", "readxl", "rlang",
              "stringr", "tibble", "tidyr", "withr", "pkgload", "testthat")
if (!"--no-app" %in% args) {
  packages <- c(packages, "shiny", "bslib", "reactable", "callr", "zip")
}
if ("--templates" %in% args) {
  packages <- c(packages, "tidyverse")
}

# A fresh R has no CRAN mirror chosen, and Rscript cannot ask for one
repos <- getOption("repos")
if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# Without admin rights the system library is read-only: install into the
# user's own library instead, as R would offer to in an interactive session
if (file.access(.libPaths()[1], 2) != 0) {
  user_lib <- Sys.getenv("R_LIBS_USER")
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(user_lib, .libPaths()))
  say("Installing into your own R library: ", user_lib)
}

missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing) == 0) {
  say("All ", length(packages), " packages are already installed.")
} else {
  say("Installing ", length(missing), " packages: ", paste(missing, collapse = ", "))
  if ("highs" %in% missing && Sys.info()[["sysname"]] == "Linux" &&
      !nzchar(Sys.which("cmake"))) {
    fail("the highs solver package needs cmake to build on Linux. Install it first ",
         "(e.g. sudo apt install cmake, sudo dnf install cmake or sudo pacman -S cmake), then run this again.")
  }
  install.packages(missing)
  still_missing <- setdiff(missing, rownames(installed.packages()))
  if (length(still_missing) > 0) {
    fail("these packages did not install: ", paste(still_missing, collapse = ", "),
         ". Scroll up for the first error message.")
  }
  say("Installed.")
}

if ("--no-check" %in% args) {
  say("\nSetup done (check skipped).")
  quit(status = 0)
}

say("\nChecking that rota-writer works (about a minute)...")
pkgload::load_all(quiet = TRUE)
inputs <- read_rota_inputs(rota_example())
problems <- validate_inputs(inputs)
if (nrow(problems) > 0) {
  print(problems)
  fail("the example inputs did not validate.")
}
say("  Example inputs read and validated.")

solution <- solve_rota(inputs, time_limit = 60)
if (is.null(solution$rota)) {
  fail("the solver found no rota for the example inputs (", solution$status, ").")
}
violations <- check_rota(solution$rota, inputs)
n_hard <- sum(violations$hard_soft == "hard")
if (n_hard > 0) {
  fail("the example rota breaks ", n_hard, " hard rules.")
}
say("  Example rota solved in ", round(solution$seconds), " s with no hard rules broken.")

say("\nSetup done. Start the app with: Rscript scripts/run_app.R")
