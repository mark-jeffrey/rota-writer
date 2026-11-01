#' rotawriter: contract-compliant on-call rotas for resident doctors
#'
#' Reads rota inputs from a spreadsheet, checks rotas against the 2016 TCS
#' for resident doctors in England, and writes rotas by
#' mixed-integer optimisation.
#'
#' The main functions are:
#' * [read_rota_inputs()] and [validate_inputs()]: load and check the inputs
#'   workbook
#' * [read_rota()]: load a rota
#' * [check_rota()]: list every rule a rota breaks
#' * [summarise_doctors()]: per-doctor totals and fair shares
#' * [summarise_patterns()]: per-doctor run lengths and wellbeing patterns
#' * [write_check_report()]: save the results as a workbook
#' * [solve_rota()]: write a rota that meets the hard rules and minimises
#'   the soft-rule penalties
#' * [write_rota()]: save a rota as a workbook for the coordinator
#' * [write_calendars()]: one `.ics` calendar per doctor
#'
#' @keywords internal
#' @import dplyr purrr tidyr
#' @importFrom rlang .data .env %||%
"_PACKAGE"
