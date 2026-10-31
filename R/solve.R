# Solving the rota model with HiGHS and writing the result.

#' Solve the rota
#'
#' Builds the model with [build_rota_model()] and solves it with the HiGHS
#' MILP solver. So far only the hard rules are modelled, so the result is a
#' compliant rota but not yet a fair one: on-calls, requests and skill mix
#' are not balanced. Check the result with [check_rota()].
#'
#' @inheritParams check_rota
#' @param time_limit Seconds before the solver stops and returns the best
#'   rota found. Defaults to `solver_time_limit_s` in the `settings` sheet.
#' @param verbose If `TRUE`, print the solver log.
#'
#' @returns A `rota_solution`: a list with
#'   \describe{
#'     \item{rota}{The rota in long format (`doctor_id`, `date`, `code`), as
#'       used by [check_rota()], or `NULL` if no rota was found.}
#'     \item{status}{The solver's status, e.g. `"Optimal"` or
#'       `"Infeasible"`.}
#'     \item{objective}{Objective value (0 while there are no soft rules).}
#'     \item{seconds}{Time spent building and solving.}
#'     \item{model}{The `rota_model` that was solved.}
#'   }
#'
#' @seealso [write_rota()] to save the rota as a workbook.
#' @export
#' @examples
#' \dontrun{
#' inputs <- read_rota_inputs(system.file("extdata", "rota_inputs.xlsx", package = "rotawriter"))
#' solution <- solve_rota(inputs)
#' check_rota(solution$rota, inputs)
#' }
solve_rota <- function(inputs, time_limit = NULL, verbose = FALSE) {
  inputs <- as_rota_inputs(inputs)
  setting <- set_names(inputs$settings$value, inputs$settings$key)
  time_limit <- time_limit %||% as.numeric(setting["solver_time_limit_s"])
  if (is.na(time_limit)) {
    time_limit <- Inf
  }
  if (!is.na(setting["solver"]) && tolower(setting[["solver"]]) != "highs") {
    warning("Only the HiGHS solver is supported so far; using it instead of ",
            setting[["solver"]], call. = FALSE)
  }

  started <- Sys.time()
  model <- build_rota_model(inputs)
  A <- Matrix::sparseMatrix(
    i = model$terms$i, j = model$terms$j, x = model$terms$coef,
    dims = c(nrow(model$rows), nrow(model$vars))
  )
  result <- highs::highs_solve(
    L = model$vars$cost, lower = model$vars$lower, upper = model$vars$upper,
    A = A, lhs = model$rows$lo, rhs = model$rows$hi, types = model$vars$type,
    control = highs::highs_control(time_limit = time_limit, log_to_console = verbose)
  )

  # A feasible point can also come back when the solver stops at the time limit
  found <- identical(result$info$primal_solution_status, "Feasible")
  rota <- NULL
  if (found) {
    rota <- model$vars |>
      filter(kind == "x", result$primal_solution[j] > 0.5) |>
      transmute(doctor_id, date, code = shift_code) |>
      arrange(date, doctor_id)
  }

  structure(
    list(
      rota = rota,
      status = result$status_message,
      objective = if (found) result$objective_value else NA_real_,
      seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      model = model
    ),
    class = c("rota_solution", "list")
  )
}

#' @export
print.rota_solution <- function(x, ...) {
  cat(sprintf("<rota_solution> %s after %.1f s\n", x$status, x$seconds))
  if (!is.null(x$rota)) {
    cat(sprintf("  %d shifts for %d doctors, %s to %s\n", nrow(x$rota),
                n_distinct(x$rota$doctor_id), min(x$rota$date), max(x$rota$date)))
  } else {
    cat("  No rota found\n")
  }
  invisible(x)
}

leave_codes <- c(annual = "AL", study = "SL", exam = "EX", parental = "PL")

#' Write a rota workbook
#'
#' Saves a rota in the same layout as `inst/extdata/example_rota_output.xlsx`,
#' so it can be read back with [read_rota()] and checked with [check_rota()]:
#' * `grid`: one row per date and one column per doctor, for people to read
#' * `long`: one row per doctor per day (`doctor_id`, `date`, `code`)
#'
#' Approved leave is added as `AL` (annual), `SL` (study), `EX` (exam) or
#' `PL` (parental) on days the doctor has no shift.
#'
#' @param rota A rota in long format, e.g. `solve_rota(inputs)$rota`.
#' @inheritParams check_rota
#' @param path Path of the `.xlsx` file to write. An existing file is
#'   overwritten.
#'
#' @returns `path`, invisibly.
#' @export
write_rota <- function(rota, inputs, path) {
  inputs <- as_rota_inputs(inputs)
  rota <- as_rota(rota)

  leave <- leave_days(inputs) |>
    filter(status == "approved") |>
    transmute(doctor_id, date, code = unname(leave_codes[leave_type])) |>
    distinct(doctor_id, date, .keep_all = TRUE)
  long <- bind_rows(rota, anti_join(leave, rota, by = c("doctor_id", "date"))) |>
    filter(date >= inputs$period$start, date <= inputs$period$end) |>
    arrange(date, doctor_id)

  grid <- long |>
    pivot_wider(names_from = doctor_id, values_from = code) |>
    right_join(tibble::tibble(date = rota_dates(inputs)), by = "date") |>
    arrange(date) |>
    mutate(day = as.character(lubridate::wday(date, label = TRUE, locale = "C")), .after = date) |>
    select(date, day, any_of(inputs$staff$doctor_id))

  add_sheet <- function(wb, name, x) {
    wb |>
      openxlsx2::wb_add_worksheet(name) |>
      openxlsx2::wb_add_data_table(name, x, table_style = "TableStyleMedium2", na.strings = "") |>
      openxlsx2::wb_freeze_pane(name, first_row = TRUE) |>
      openxlsx2::wb_set_col_widths(name, cols = seq_along(x), widths = "auto")
  }
  grid_dims <- openxlsx2::wb_dims(rows = seq_len(nrow(grid)) + 1, cols = 3:max(3, ncol(grid)))
  highlight <- function(wb, text, style) {
    openxlsx2::wb_add_conditional_formatting(wb, "grid", dims = grid_dims, type = "containsText",
                                             rule = text, style = style)
  }

  wb <- openxlsx2::wb_workbook(creator = "rota-writer") |>
    openxlsx2::wb_add_dxfs_style(name = "night", bg_fill = openxlsx2::wb_color("FF3F3F7F"),
                                 font_color = openxlsx2::wb_color("FFFFFFFF")) |>
    openxlsx2::wb_add_dxfs_style(name = "long_day", bg_fill = openxlsx2::wb_color("FFFFE08A")) |>
    openxlsx2::wb_add_dxfs_style(name = "leave", bg_fill = openxlsx2::wb_color("FFB7E1CD")) |>
    add_sheet("grid", grid) |>
    openxlsx2::wb_freeze_pane("grid", first_active_row = 2, first_active_col = 3) |>
    highlight("_N", "night") |>
    highlight("_LD", "long_day")
  for (code in leave_codes) {
    wb <- highlight(wb, code, "leave")
  }
  wb <- add_sheet(wb, "long", long)

  openxlsx2::wb_save(wb, path, overwrite = TRUE)
  invisible(path)
}
