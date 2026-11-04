# Take the app screenshots in the README.
#
#   Rscript data-raw/make_screenshots.R
#
# Writes docs/images/app-results.png (the by-role view) and
# docs/images/app-by-doctor.png: the example inputs, with the solved example
# rota uploaded to the Check a rota tab. Needs shinytest2 and Chrome or
# Chromium (set CHROMOTE_CHROME if it is not found). The Solve tab's
# screenshot, docs/images/app-solve.png, is of a solve in progress and is
# taken by hand.

pkgload::load_all(quiet = TRUE)
Sys.setenv(NOT_CRAN = "true") # AppDriver otherwise refuses to start

# The app runs in its own R process, so it loads rota-writer from here itself
app_dir <- tempfile("rota-app-")
dir.create(app_dir)
writeLines(c(sprintf('pkgload::load_all("%s", quiet = TRUE)', normalizePath(".")), "rota_app()"),
           file.path(app_dir, "app.R"))
app <- shinytest2::AppDriver$new(app_dir, width = 1400, height = 900, seed = 1,
                                 load_timeout = 60 * 1000)
on.exit(app$stop())

app$click("inputs-example")
app$wait_for_idle()
app$set_inputs(tabs = "check")
app$upload_file(`check-file` = rota_example("residents", "output/rota.xlsx"))
app$wait_for_idle(timeout = 60 * 1000)
screenshot <- function(path) {
  unlink(path) # get_screenshot() won't overwrite
  app$get_screenshot(path, selector = "viewport")
}
screenshot("docs/images/app-results.png")

# The results' tabs have no input id, so click the tab by its name
app$run_js("[...document.querySelectorAll('.tab-pane[data-value=check] .nav-link')]
  .find(a => a.textContent.trim() === 'By doctor').click();")
app$wait_for_idle()
screenshot("docs/images/app-by-doctor.png")

message("Wrote docs/images/app-results.png and docs/images/app-by-doctor.png")
