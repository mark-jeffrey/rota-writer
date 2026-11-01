# Run the rota-writer app from the source folder.
#
#   Rscript scripts/run_app.R [port]
#
# Run from the project root. Opens the app in the default web browser.

pkgload::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
run_app(port = if (length(args) >= 1) as.integer(args[1]))
