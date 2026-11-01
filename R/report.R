# Writing the compliance report.

#' Write a compliance report workbook
#'
#' Saves the results of [check_rota()] and [summarise_doctors()] as an Excel
#' workbook for rota coordinators, with hard violations shaded red and soft
#' ones yellow. It has three sheets, and a fourth if `patterns` is given:
#' * `summary`: violations and total penalty per rule
#' * `violations`: one row per violation
#' * `doctors`: per-doctor hours against contracted hours, and on-calls,
#'   nights and weekends against fair shares. The differences
#'   (`hours_vs_contracted`, in hours a week, and `*_vs_fair`) are shaded
#'   blue (fewer) to red (more)
#' * `patterns`: per-doctor run lengths, single nights, split weekends and
#'   single days off, with department totals
#'
#' @param violations Output of [check_rota()].
#' @param doctors Output of [summarise_doctors()].
#' @param patterns Output of [summarise_patterns()], or `NULL` to leave the
#'   sheet out.
#' @param path Path of the `.xlsx` file to write. An existing file is
#'   overwritten.
#'
#' @returns `path`, invisibly.
#'
#' @export
#' @examples
#' inputs <- read_rota_inputs(system.file("extdata", "rota_inputs.xlsx", package = "rotawriter"))
#' rota <- read_rota(system.file("extdata", "example_rota_output.xlsx", package = "rotawriter"))
#'
#' write_check_report(
#'   check_rota(rota, inputs),
#'   summarise_doctors(rota, inputs),
#'   tempfile(fileext = ".xlsx"),
#'   patterns = summarise_patterns(rota, inputs)
#' )
write_check_report <- function(violations, doctors, path, patterns = NULL) {
  summary <- violations |>
    summarise(n = n(), doctors = n_distinct(doctor_id, na.rm = TRUE),
              penalty = sum(penalty), .by = c(hard_soft, rule_id)) |>
    arrange(desc(hard_soft == "hard"), desc(n))

  wb <- openxlsx2::wb_workbook(creator = "rota-writer") |>
    openxlsx2::wb_add_dxfs_style(name = "hard", bg_fill = openxlsx2::wb_color("FFF4C7C3")) |>
    openxlsx2::wb_add_dxfs_style(name = "soft", bg_fill = openxlsx2::wb_color("FFFFF2CC"))

  measures <- intersect(c("on_calls", "nights", "weekends"), names(doctors))
  measures <- measures[paste0("fair_", measures) %in% names(doctors)]
  for (m in measures) {
    doctors[[paste0(m, "_vs_fair")]] <- round(doctors[[m]] - doctors[[paste0("fair_", m)]], 1)
  }
  diffs <- paste0(measures, "_vs_fair")
  if (all(c("avg_weekly_hours", "contracted_hours") %in% names(doctors))) {
    doctors <- doctors |>
      mutate(hours_vs_contracted = round(avg_weekly_hours - contracted_hours, 1),
             .after = avg_weekly_hours)
    diffs <- c("hours_vs_contracted", diffs)
  }

  sheets <- list(summary = summary, violations = violations, doctors = doctors, patterns = patterns) |>
    compact()
  for (name in names(sheets)) {
    x <- sheets[[name]]
    wb <- wb |>
      openxlsx2::wb_add_worksheet(name) |>
      openxlsx2::wb_add_data_table(name, x, table_style = "TableStyleMedium2", na.strings = "") |>
      openxlsx2::wb_freeze_pane(name, first_row = TRUE) |>
      openxlsx2::wb_set_col_widths(name, cols = seq_along(x), widths = "auto")
    if ("hard_soft" %in% names(x) && nrow(x) > 0) {
      dims <- openxlsx2::wb_dims(rows = seq_len(nrow(x)) + 1, cols = seq_along(x))
      col <- openxlsx2::int2col(match("hard_soft", names(x)))
      for (level in c("hard", "soft")) {
        wb <- openxlsx2::wb_add_conditional_formatting(
          wb, name, dims = dims, rule = sprintf('$%s2="%s"', col, level), style = level
        )
      }
    }
  }

  for (col in match(diffs, names(doctors))) {
    wb <- openxlsx2::wb_add_conditional_formatting(
      wb, "doctors", dims = openxlsx2::wb_dims(rows = seq_len(max(nrow(doctors), 1)) + 1, cols = col),
      type = "colorScale", style = c("FF5A8AC6", "FFFFFFFF", "FFF8696B"), rule = c(-4, 0, 4)
    )
  }

  openxlsx2::wb_save(wb, path, overwrite = TRUE)
  invisible(path)
}
