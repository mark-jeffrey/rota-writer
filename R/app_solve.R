# Solve tab: runs solve_rota() in a background R process so the app stays
# responsive. The process and the app share a folder holding:
#   inputs.rds    the inputs, written by the app
#   progress.csv  seconds,objective lines from solve_rota()'s progress callback
#   stop          created by the app to stop early and keep the best rota
#   solution.rds  the rota_solution (without its model), written at the end
#   stderr.txt    the process's messages and errors

solve_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Solve",
      width = 320,
      shiny::numericInput(ns("minutes"), "Time limit (minutes)", value = 10, min = 1, step = 1),
      shiny::helpText("Longer gives a better rota. The settings sheet's solver_time_limit_s",
                      "sets the starting value."),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Advanced",
          shiny::numericInput(ns("seed"), "Random seed", value = 1, min = 1, step = 1),
          shiny::helpText("The same inputs and seed give the same rota.")
        )
      ),
      shiny::uiOutput(ns("buttons"))
    ),
    shiny::uiOutput(ns("status")),
    shiny::plotOutput(ns("history"), height = "320px")
  )
}

# Returns a reactive holding the latest rota_solution, or NULL
solve_server <- function(id, loaded) {
  shiny::moduleServer(id, function(input, output, session) {
    job <- shiny::reactiveVal(NULL) # list(process, dir, started, time_limit)
    state <- shiny::reactiveVal("idle") # idle, running, stopping, done, cancelled, failed
    history <- shiny::reactiveVal(NULL)
    error <- shiny::reactiveVal(NULL)
    solution <- shiny::reactiveVal(NULL)

    shiny::observeEvent(loaded(), {
      limit <- setting_value(loaded()$inputs, "solver_time_limit_s")
      if (!is.na(limit)) shiny::updateNumericInput(session, "minutes", value = round(limit / 60, 1))
    })

    output$buttons <- shiny::renderUI({
      ns <- session$ns
      x <- loaded()
      if (state() %in% c("running", "stopping")) {
        return(shiny::div(
          class = "d-grid gap-2",
          disabled(shiny::actionButton(ns("stop"), "Stop and keep best rota", class = "btn-primary"),
                   when = state() == "stopping"),
          shiny::actionButton(ns("cancel"), "Cancel", class = "btn-outline-danger")
        ))
      }
      shiny::div(
        class = "d-grid gap-2",
        disabled(shiny::actionButton(ns("solve"), "Solve", class = "btn-primary"),
                 when = is.null(x) || !x$ok),
        if (is.null(x)) shiny::helpText("Load the inputs workbook first, on the Inputs tab.")
        else if (!x$ok) shiny::helpText("Fix the problems listed on the Inputs tab first.")
      )
    })

    shiny::observeEvent(input$solve, {
      x <- loaded()
      shiny::req(x$ok, input$minutes > 0, state() != "running")
      job(start_background_solve(x$inputs, time_limit = input$minutes * 60,
                                 seed = input$seed %||% 1))
      history(NULL)
      error(NULL)
      state("running")
    })

    shiny::observeEvent(input$stop, {
      file.create(file.path(job()$dir, "stop"))
      state("stopping")
    })

    shiny::observeEvent(input$cancel, {
      job()$process$kill()
      state("cancelled")
    })

    # Follow the running solve once a second
    shiny::observe({
      shiny::req(state() %in% c("running", "stopping"))
      shiny::invalidateLater(1000)
      j <- job()
      history(read_progress(j$dir))
      if (j$process$is_alive()) {
        return()
      }
      result <- file.path(j$dir, "solution.rds")
      if (file.exists(result)) {
        solution(readRDS(result))
        state("done")
      } else {
        error(read_text(file.path(j$dir, "stderr.txt")))
        state("failed")
      }
    })

    session$onSessionEnded(function() {
      j <- shiny::isolate(job())
      if (!is.null(j) && j$process$is_alive()) j$process$kill()
    })

    output$status <- shiny::renderUI({
      shiny::invalidateLater(if (state() %in% c("running", "stopping")) 1000 else 60000)
      j <- job()
      h <- history()
      best <- if (!is.null(h) && nrow(h) > 0) format(round(utils::tail(h$objective, 1)), big.mark = ",")
      switch(
        state(),
        idle = app_note("Load the inputs, choose a time limit and press Solve."),
        running = ,
        stopping = {
          elapsed <- as.numeric(difftime(Sys.time(), j$started, units = "secs"))
          shiny::tagList(
            progress_bar(min(elapsed / j$time_limit, 1)),
            shiny::p(sprintf("%s of %s.", format_minutes(elapsed), format_minutes(j$time_limit)),
                     if (is.null(best)) "Building the model and finding a first rota..."
                     else paste("Best penalty so far:", best),
                     if (state() == "stopping") "Stopping after this step...")
          )
        },
        done = {
          s <- solution()
          shiny::tagList(
            shiny::p(shiny::strong(s$status), sprintf("after %s.", format_minutes(s$seconds)),
                     "Penalty", format(round(s$objective), big.mark = ","), "-",
                     "see the Results tab."),
            if (nrow(s$unfilled) > 0) {
              shiny::p(class = "text-danger",
                       nrow(s$unfilled), "slots could not be filled without breaking a hard rule.")
            }
          )
        },
        cancelled = app_note("Cancelled. Press Solve to start again."),
        failed = shiny::tagList(
          shiny::p(class = "text-danger", "The solver stopped with an error:"),
          shiny::pre(error())
        )
      )
    })

    output$history <- shiny::renderPlot({
      h <- history()
      shiny::req(!is.null(h), nrow(h) > 0)
      graphics::par(mar = c(4, 5, 1, 1))
      plot(h$seconds / 60, h$objective, type = "s", lwd = 2, col = "#2C6FB7", las = 1,
           xlab = "Minutes", ylab = "", xlim = c(0, max(job()$time_limit, h$seconds) / 60))
      graphics::mtext("Penalty (lower is better)", side = 2, line = 4)
    })

    solution
  })
}

# Start solve_rota() in a background R process
start_background_solve <- function(inputs, time_limit, seed) {
  dir <- tempfile("rota-solve-")
  dir.create(dir)
  saveRDS(inputs, file.path(dir, "inputs.rds"))
  process <- callr::r_bg(
    background_solve,
    args = list(dir = dir, time_limit = time_limit, seed = seed, dev_path = dev_package_path()),
    stdout = file.path(dir, "stdout.txt"),
    stderr = file.path(dir, "stderr.txt"),
    supervise = TRUE
  )
  list(process = process, dir = dir, started = Sys.time(), time_limit = time_limit)
}

# Runs in the background process, so it is self-contained: callr runs a copy
# of it without this package's namespace
background_solve <- function(dir, time_limit, seed, dev_path) {
  if (is.null(dev_path)) {
    loadNamespace("rotawriter")
  } else {
    pkgload::load_all(dev_path, quiet = TRUE)
  }
  progress_file <- file.path(dir, "progress.csv")
  stop_file <- file.path(dir, "stop")
  solution <- rotawriter::solve_rota(
    readRDS(file.path(dir, "inputs.rds")), time_limit = time_limit, seed = seed,
    progress = function(seconds, objective) {
      cat(sprintf("%.1f,%.10g\n", seconds, objective), file = progress_file, append = TRUE)
      file.exists(stop_file)
    }
  )
  solution$model <- NULL
  saveRDS(solution, file.path(dir, "solution.rds"))
}

# The source folder if the package was loaded with pkgload::load_all(), so the
# background process loads the same code; NULL if it is installed
dev_package_path <- function() {
  if (requireNamespace("pkgload", quietly = TRUE) && pkgload::is_dev_package("rotawriter")) {
    pkgload::pkg_path(getNamespaceInfo("rotawriter", "path"))
  }
}

read_progress <- function(dir) {
  path <- file.path(dir, "progress.csv")
  if (!file.exists(path) || file.size(path) == 0) {
    return(NULL)
  }
  lines <- readLines(path, warn = FALSE)
  values <- do.call(rbind, strsplit(lines[grepl("^[0-9.]+,[0-9.e+-]+$", lines)], ",", fixed = TRUE))
  if (is.null(values)) {
    return(NULL)
  }
  tibble::tibble(seconds = as.numeric(values[, 1]), objective = as.numeric(values[, 2]))
}

read_text <- function(path) {
  if (file.exists(path)) paste(readLines(path, warn = FALSE), collapse = "\n") else ""
}

setting_value <- function(inputs, key) {
  value <- inputs$settings$value[match(key, inputs$settings$key)]
  suppressWarnings(as.numeric(value))
}

format_minutes <- function(seconds) {
  if (seconds < 60) sprintf("%.0f s", seconds) else sprintf("%.1f min", seconds / 60)
}

progress_bar <- function(fraction) {
  shiny::div(
    class = "progress mb-2", role = "progressbar",
    shiny::div(class = "progress-bar progress-bar-striped progress-bar-animated",
               style = sprintf("width: %.0f%%", 100 * fraction))
  )
}
