# The point-and-click app: upload the inputs workbook, solve, view the rota
# and its checks, and download the coordinator's files. It runs on the
# coordinator's own computer, so no staff data leaves it. Each tab is a Shiny
# module; solving runs in a background R process (R/app_solve.R), so the app
# stays responsive during a long solve.

#' Run the rota-writer app
#'
#' Opens the rota-writer app in a web browser on this computer. In the app
#' you can:
#' * upload an inputs workbook (or use the example data) and see any
#'   problems [validate_inputs()] finds
#' * solve the rota with [solve_rota()], watching the penalty fall, and stop
#'   early keeping the best rota so far
#' * view the rota, who is on each shift, fairness, patterns and every rule
#'   broken, as [check_rota()] reports them
#' * check a rota edited by hand
#' * download the rota workbook, the compliance report and per-doctor
#'   calendars
#'
#' Nothing is sent anywhere: the app and the solver run on this computer.
#' Close the browser tab and press Esc (or Ctrl+C) in R to stop it.
#'
#' @param launch.browser Open the app in the default web browser.
#' @param port Port to serve the app on. By default Shiny picks one.
#'
#' @returns Nothing; runs until the app is stopped.
#' @export
#' @examples
#' \dontrun{
#' run_app()
#' }
run_app <- function(launch.browser = TRUE, port = NULL) {
  rlang::check_installed(app_packages, reason = "to run the app")
  shiny::runApp(rota_app(), launch.browser = if (isTRUE(launch.browser)) open_browser else launch.browser,
                port = port)
}

# Open the app's address in a browser. Under Rscript R often has no browser
# set (on Linux, unless R_BROWSER is), so fall back to the desktop's default
# browser, or else ask the user to open the address themselves.
open_browser <- function(url) {
  if (.Platform$OS.type == "windows") {
    return(utils::browseURL(url)) # always opens the default browser
  }
  browser <- browser_command()
  if (is.null(browser)) {
    message("Open ", url, " in your web browser to use the app.")
  } else {
    utils::browseURL(url, browser = browser)
  }
}

# The browser option if set, else the desktop's "open" command, else NULL
browser_command <- function(browser = getOption("browser")) {
  if (is.function(browser) || (is.character(browser) && length(browser) == 1 && nzchar(browser))) {
    return(browser)
  }
  for (opener in c("xdg-open", "open")) {
    if (nzchar(Sys.which(opener))) return(opener)
  }
  NULL
}

app_packages <- c("shiny", "bslib", "reactable", "callr", "zip")

rota_app <- function() {
  shiny::shinyApp(app_ui(), app_server)
}

app_ui <- function() {
  bslib::page_navbar(
    title = "rota-writer",
    id = "tabs",
    theme = bslib::bs_theme(version = 5),
    bslib::nav_panel("Inputs", value = "inputs", inputs_ui("inputs")),
    bslib::nav_panel("Solve", value = "solve", solve_ui("solve")),
    bslib::nav_panel("Results", value = "results", results_ui("results")),
    bslib::nav_panel("Check a rota", value = "check", check_ui("check")),
    header = disconnected_banner()
  )
}

# Shiny's own sign that the page has lost R (a faint grey wash) is easy to
# miss, and the page then looks normal but does nothing, so say so plainly
disconnected_banner <- function() {
  shiny::tagList(
    shiny::div(
      id = "rota-disconnected", class = "alert alert-danger m-3", style = "display: none;",
      shiny::strong("The app has stopped."),
      "This page is no longer connected to R, so nothing on it will work.",
      "Start the app again (Rscript scripts/run_app.R or run_app()) and use the new tab."
    ),
    shiny::tags$script(shiny::HTML(
      "$(document).on('shiny:disconnected', function() {
         document.getElementById('rota-disconnected').style.display = 'block';
       });"
    ))
  )
}

app_server <- function(input, output, session) {
  loaded <- inputs_server("inputs")
  inputs <- shiny::reactive(loaded()$inputs)
  solution <- solve_server("solve", loaded)
  results_server("results", inputs, rota = shiny::reactive(solution()$rota), solution = solution)
  check_server("check", inputs)

  # Show the rota as soon as a solve finishes
  shiny::observeEvent(solution(), bslib::nav_select("tabs", "results"))
}

# Small helpers -----------------------------------------------------------

app_table <- function(x, ..., defaultPageSize = 20) {
  reactable::reactable(x, compact = TRUE, striped = TRUE, highlight = TRUE,
                       defaultPageSize = defaultPageSize, ...)
}

# A muted note in place of output that isn't ready yet
app_note <- function(...) {
  shiny::p(class = "text-muted", ...)
}

disabled <- function(tag, when = TRUE) {
  if (when) shiny::tagAppendAttributes(tag, disabled = NA) else tag
}

# Inputs tab --------------------------------------------------------------

inputs_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Inputs workbook",
      width = 320,
      shiny::fileInput(ns("file"), NULL, accept = ".xlsx", placeholder = "rota_inputs.xlsx"),
      shiny::actionButton(ns("example"), "Use the example data", class = "btn-outline-secondary"),
      shiny::helpText("Edit staff, leave, requests and rules in Excel, then choose the workbook",
                      "here: it loads as soon as you pick it. It stays on this computer.")
    ),
    shiny::uiOutput(ns("summary"))
  )
}

# Returns a reactive: NULL until a workbook loads, then a list with the
# `inputs`, the validation `problems`, the file `name`, and `ok` (no problems)
inputs_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    loaded <- shiny::reactiveVal(NULL)
    load <- function(path, name) {
      inputs <- tryCatch(read_rota_inputs(path), error = \(e) e)
      if (inherits(inputs, "error")) {
        shiny::showNotification(paste("Couldn't read", name, "-", conditionMessage(inputs)),
                                type = "error", duration = NULL)
        return()
      }
      problems <- validate_inputs(inputs)
      loaded(list(inputs = inputs, problems = problems, name = name, ok = nrow(problems) == 0))
    }

    shiny::observeEvent(input$file, load(input$file$datapath, input$file$name))
    shiny::observeEvent(input$example, {
      load(system.file("extdata", "rota_inputs.xlsx", package = "rotawriter"), "Example data")
    })

    output$summary <- shiny::renderUI({
      x <- loaded()
      if (is.null(x)) {
        return(app_note("Upload an inputs workbook to start."))
      }
      inputs <- x$inputs
      sheets <- setdiff(names(inputs), "period")
      pools <- inputs$staff |>
        mutate(pool = staff_pool(inputs)) |>
        summarise(doctors = n(), wte = sum(wte), .by = pool)

      shiny::tagList(
        bslib::layout_column_wrap(
          width = 1 / 3,
          bslib::value_box("Workbook", x$name),
          bslib::value_box("Rota", paste(format(inputs$period$start, "%d %b %Y"), "to",
                                         format(inputs$period$end, "%d %b %Y"))),
          bslib::value_box("Doctors", nrow(inputs$staff),
                           paste(nrow(pools), if (nrow(pools) == 1) "staffing pool" else "staffing pools"))
        ),
        if (x$ok) {
          bslib::card(class = "border-success", bslib::card_body(
            "No problems found. Go to Solve to write the rota, or Check a rota to check one."
          ))
        } else {
          bslib::card(
            class = "border-danger",
            bslib::card_header(sprintf("%d problems to fix in the workbook before solving",
                                       nrow(x$problems))),
            app_table(x$problems)
          )
        },
        bslib::layout_column_wrap(
          width = 1 / 2,
          bslib::card(bslib::card_header("Staffing pools"),
                      app_table(mutate(pools, pool = if_else(pool == "", "(all)", pool)))),
          bslib::card(bslib::card_header("Sheets"),
                      app_table(tibble::tibble(sheet = sheets,
                                               rows = map_int(unclass(inputs)[sheets], nrow)),
                                defaultPageSize = 15))
        )
      )
    })

    loaded
  })
}

# Check a rota tab --------------------------------------------------------

check_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Rota workbook",
      width = 320,
      shiny::fileInput(ns("file"), NULL, accept = ".xlsx", placeholder = "rota.xlsx"),
      shiny::helpText("Upload a rota to check against the loaded inputs, such as one edited by",
                      "hand. It needs a \"long\" sheet, as rota workbooks from this app have.")
    ),
    results_ui(ns("results"))
  )
}

check_server <- function(id, inputs) {
  shiny::moduleServer(id, function(input, output, session) {
    rota <- shiny::eventReactive(input$file, {
      shiny::validate(shiny::need(inputs(), "Load the inputs workbook first, on the Inputs tab."))
      rota <- tryCatch(read_rota(input$file$datapath), error = \(e) e)
      if (inherits(rota, "error")) {
        shiny::validate(paste("Couldn't read the rota:", conditionMessage(rota)))
      }
      rota
    })
    results_server("results", inputs, rota = rota, solution = shiny::reactive(NULL))
  })
}
