# Results views, used for a solved rota (Results tab) and an uploaded one
# (Check a rota tab): summary, the rota grid, who is on each shift, fairness,
# patterns and every rule broken, with the coordinator's downloads.

results_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("summary")),
    bslib::navset_card_underline(
      bslib::nav_panel("Rota", reactable::reactableOutput(ns("grid"))),
      bslib::nav_panel("By shift", reactable::reactableOutput(ns("by_shift"))),
      bslib::nav_panel("Fairness", shiny::uiOutput(ns("fairness_note")),
                       reactable::reactableOutput(ns("fairness"))),
      bslib::nav_panel("Patterns", reactable::reactableOutput(ns("patterns"))),
      bslib::nav_panel("Rules broken", reactable::reactableOutput(ns("violations"))),
      bslib::nav_panel(
        "Download",
        shiny::p("Files for the coordinator, the same as the command-line solver writes."),
        shiny::div(
          class = "d-flex flex-wrap gap-2",
          shiny::downloadButton(ns("rota_xlsx"), "Rota workbook (rota.xlsx)"),
          shiny::downloadButton(ns("report_xlsx"), "Compliance report (report.xlsx)"),
          shiny::downloadButton(ns("calendars_zip"), "Calendars (calendars.zip)")
        )
      )
    )
  )
}

# `rota` is a reactive long-format rota; `solution` a reactive rota_solution
# or NULL (for an uploaded rota)
results_server <- function(id, inputs, rota, solution) {
  shiny::moduleServer(id, function(input, output, session) {
    ready <- shiny::reactive({
      shiny::validate(
        shiny::need(inputs(), "Load the inputs workbook first, on the Inputs tab."),
        shiny::need(rota(), "No rota yet: solve one on the Solve tab.")
      )
      list(inputs = inputs(), rota = rota())
    })
    violations <- shiny::reactive(check_rota(ready()$rota, ready()$inputs))
    doctors <- shiny::reactive(summarise_doctors(ready()$rota, ready()$inputs))
    patterns <- shiny::reactive(summarise_patterns(ready()$rota, ready()$inputs))

    output$summary <- shiny::renderUI({
      v <- violations()
      s <- solution()
      cover <- sum(v$rule_id == "COVER")
      hard <- sum(v$hard_soft == "hard")
      bslib::layout_column_wrap(
        width = 1 / 4,
        bslib::value_box("Rota", if (is.null(s)) "Uploaded" else s$status,
                         if (!is.null(s)) sprintf("Solved in %s", format_minutes(s$seconds))),
        bslib::value_box("Hard rules broken", hard,
                         if (hard == 0) "None: the rota is compliant" else "Must be fixed",
                         theme = if (hard == 0) "success" else "danger"),
        bslib::value_box("Soft rules penalty", format(sum(v$penalty, na.rm = TRUE), big.mark = ","),
                         sprintf("%d soft violations", sum(v$hard_soft == "soft"))),
        bslib::value_box("Cover", if (cover == 0) "Every shift covered" else paste(cover, "shifts not covered"),
                         if (cover > 0) "See COVER under Rules broken",
                         theme = if (cover == 0) "success" else "danger")
      )
    })

    output$grid <- reactable::renderReactable({
      grid <- rota_grid(ready()$rota, ready()$inputs)
      doctors <- setdiff(names(grid), c("date", "day", "holiday"))
      weekend <- grid$day %in% c("Sat", "Sun")
      code_cell <- reactable::colDef(minWidth = 80, align = "center", style = function(value) {
        c(list(whiteSpace = "nowrap", fontSize = "0.85em"), cell_style(code_style(value, ready()$inputs$shifts)))
      })
      reactable::reactable(
        grid,
        pagination = FALSE, compact = TRUE, bordered = TRUE, height = 650,
        defaultColDef = reactable::colDef(headerStyle = list(fontSize = "0.8em")),
        columns = c(
          list(
            date = reactable::colDef(name = "Date", sticky = "left", minWidth = 100,
                                     style = date_style(grid)),
            day = reactable::colDef(name = "Day", sticky = "left", minWidth = 50,
                                    style = date_style(grid)),
            holiday = reactable::colDef(show = FALSE)
          ),
          set_names(rep(list(code_cell), length(doctors)), doctors)
        )
      )
    })

    output$by_shift <- reactable::renderReactable({
      x <- shift_table(ready()$rota, ready()$inputs, rota_days(ready()$inputs))
      shifts <- setdiff(names(x), c("date", "day", "holiday"))
      who_cell <- reactable::colDef(minWidth = 140, style = function(value) {
        if (!is.na(value) && grepl("UNFILLED", value)) cell_style("unfilled")
      })
      reactable::reactable(
        x,
        pagination = FALSE, compact = TRUE, bordered = TRUE, height = 650,
        columns = c(
          list(
            date = reactable::colDef(name = "Date", sticky = "left", minWidth = 100,
                                     style = date_style(x)),
            day = reactable::colDef(name = "Day", sticky = "left", minWidth = 50,
                                    style = date_style(x)),
            holiday = reactable::colDef(name = "Holiday", minWidth = 110)
          ),
          set_names(rep(list(who_cell), length(shifts)), shifts)
        )
      )
    })

    output$fairness_note <- shiny::renderUI({
      app_note("Each doctor against their contracted hours (h a week) and their WTE-weighted",
               "fair share of their pool's on-calls, nights and weekends. Blue is fewer, red",
               "more.")
    })

    output$fairness <- reactable::renderReactable({
      x <- fairness_table(doctors()) |>
        left_join(select(ready()$inputs$staff, doctor_id, name), by = "doctor_id") |>
        relocate(name, .after = doctor_id) |>
        left_join(placement_summary(ready()$inputs), by = "doctor_id") |>
        relocate(placement, .after = pool) |>
        select(-any_of(c("from", "to", "opted_out_wtr")))
      if (all(x$pool == "")) x$pool <- NULL
      columns <- map(set_names(names(x)), \(col) {
        reactable::colDef(
          name = stringr::str_replace_all(col, "_", " "),
          minWidth = if (col %in% c("name", "placement")) 150 else if (col == "pool") 110 else 80,
          sticky = if (col == "name") "left",
          style = if (col %in% fairness_diffs(names(x))) \(value) diff_style(value)
        )
      })
      app_table(x, pagination = FALSE, height = 650, filterable = TRUE, columns = columns,
                defaultColDef = reactable::colDef(headerStyle = list(fontSize = "0.85em")))
    })

    output$patterns <- reactable::renderReactable({
      app_table(patterns(), pagination = FALSE, height = 650)
    })

    output$violations <- reactable::renderReactable({
      v <- violations()
      app_table(
        v,
        filterable = TRUE, defaultPageSize = 25,
        columns = list(
          hard_soft = reactable::colDef(name = "Type", maxWidth = 80, style = function(value) {
            if (value == "hard") cell_style("unfilled")
          }),
          detail = reactable::colDef(minWidth = 300)
        )
      )
    })

    output$rota_xlsx <- shiny::downloadHandler(
      filename = "rota.xlsx",
      content = function(file) write_rota(ready()$rota, ready()$inputs, file)
    )
    output$report_xlsx <- shiny::downloadHandler(
      filename = "report.xlsx",
      content = function(file) write_check_report(violations(), doctors(), file, patterns = patterns())
    )
    output$calendars_zip <- shiny::downloadHandler(
      filename = "calendars.zip",
      content = function(file) {
        dir <- tempfile("calendars-")
        on.exit(unlink(dir, recursive = TRUE))
        write_calendars(ready()$rota, ready()$inputs, dir)
        zip::zip(file, files = list.files(dir), root = dir)
      }
    )
  })
}

# CSS for a cell in one of the rota_colours styles (NA for none)
cell_style <- function(style) {
  colours <- rota_colours[match(style, rota_colours$style), ]
  if (length(style) == 0 || is.na(style) || nrow(colours) == 0) {
    return(NULL)
  }
  compact(list(background = colours$fill, color = if (!is.na(colours$text)) colours$text))
}

# Date columns shaded for weekends and bank holidays, as in the workbook
date_style <- function(x) {
  function(value, index) {
    if (!is.na(x$holiday[index])) cell_style("holiday")
    else if (x$day[index] %in% c("Sat", "Sun")) cell_style("weekend")
  }
}

# Blue for below 0, red above, stronger further away (full at 4), as in the
# compliance report
diff_style <- function(value) {
  if (is.na(value) || value == 0) {
    return(NULL)
  }
  strength <- min(abs(value) / 4, 1)
  end <- if (value < 0) c(90, 138, 198) else c(248, 105, 107)
  rgb <- round(255 + (end - 255) * strength)
  list(background = sprintf("rgb(%d, %d, %d)", rgb[1], rgb[2], rgb[3]))
}
