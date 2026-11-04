# Refresh the UK bank holidays shipped with rota-writer from GOV.UK.
#
#   Rscript data-raw/update_bank_holidays.R
#
# Downloads https://www.gov.uk/bank-holidays.json (England and Wales,
# Scotland, Northern Ireland) and writes inst/extdata/uk-bank-holidays.csv,
# which uk_bank_holidays() reads. GOV.UK lists about two years ahead, so run
# this once a year or so, and when a one-off holiday is announced. Only this
# script uses the internet: solving never does.

suppressPackageStartupMessages(library(dplyr))

url <- "https://www.gov.uk/bank-holidays.json"
raw <- jsonlite::fromJSON(url)

holidays <- purrr::imap(raw, \(x, nation) {
  tibble::as_tibble(x$events) |>
    transmute(
      nation,
      date = as.Date(date),
      # Straight apostrophes, and substitute days named as such
      name = gsub("’", "'", title, fixed = TRUE),
      name = if_else(notes == "Substitute day", paste(name, "(substitute)"), name)
    )
}) |>
  bind_rows() |>
  arrange(nation, date)

path <- "inst/extdata/uk-bank-holidays.csv"
write.csv(holidays, path, row.names = FALSE, fileEncoding = "UTF-8")
message(sprintf("Wrote %d bank holidays to %s, up to %s", nrow(holidays), path, max(holidays$date)))
