# UK bank holidays, from a copy of GOV.UK's list shipped with the package.

uk_nations <- c(
  "england-and-wales" = "england-and-wales", england = "england-and-wales",
  wales = "england-and-wales", scotland = "scotland", "northern-ireland" = "northern-ireland"
)

#' UK bank holidays
#'
#' Lists the bank holidays in one UK nation between two dates, in the shape
#' of the inputs workbook's `bank_holidays` sheet. They come from a copy of
#' [GOV.UK's list](https://www.gov.uk/bank-holidays) shipped with rota-writer,
#' so no internet connection is needed. Substitute days, such as a Monday off
#' when Christmas falls on a Saturday, are named as such.
#'
#' GOV.UK lists about two years ahead, and a one-off holiday (such as a
#' coronation) may be announced at short notice, so check the list for the
#' rota's dates. The copy is refreshed with `data-raw/update_bank_holidays.R`.
#' Trusts in Scotland and Northern Ireland may also give local public
#' holidays that aren't bank holidays: add those to the sheet by hand.
#'
#' @param start,end First and last dates of the rota, as dates or
#'   `"YYYY-MM-DD"` strings.
#' @param nation `"england-and-wales"` (or `"england"` or `"wales"`),
#'   `"scotland"` or `"northern-ireland"`.
#'
#' @returns A tibble with columns `date` and `name`. It warns if the rota runs
#'   past the end of the list.
#' @export
#' @examples
#' uk_bank_holidays("2027-01-01", "2027-12-31")
#' uk_bank_holidays("2027-01-01", "2027-12-31", nation = "scotland")
uk_bank_holidays <- function(start, end, nation = "england-and-wales") {
  start <- as.Date(start)
  end <- as.Date(end)
  if (length(nation) != 1 || !nation %in% names(uk_nations)) {
    stop("`nation` must be one of ", paste0('"', unique(uk_nations), '"', collapse = ", "), ".",
         call. = FALSE)
  }
  path <- system.file("extdata", "uk-bank-holidays.csv", package = "rotawriter")
  all <- utils::read.csv(path, encoding = "UTF-8", colClasses = "character") |>
    tibble::as_tibble() |>
    filter(nation == uk_nations[[!!nation]]) |>
    mutate(date = as.Date(date))

  if (end > max(all$date)) {
    warning(sprintf(
      "GOV.UK's bank holidays in rota-writer end on %s, before the rota ends on %s. Add later ones by hand, or refresh the list with data-raw/update_bank_holidays.R.",
      format(max(all$date)), format(end)
    ), call. = FALSE)
  }
  all |>
    filter(date >= start, date <= end) |>
    select(date, name)
}
