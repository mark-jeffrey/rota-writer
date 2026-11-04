skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("reactable")
skip_if_not_installed("callr")
skip_if_not_installed("zip")

example_inputs_path <- function() rota_example()

test_that("the app builds", {
  expect_s3_class(rota_app(), "shiny.appobj")
})

test_that("the inputs tab loads a workbook and reports its problems", {
  shiny::testServer(inputs_server, {
    session$setInputs(example = 1)
    expect_true(loaded()$ok)
    expect_equal(nrow(loaded()$inputs$staff), 42)

    # A leave row for an unknown doctor makes validation fail
    path <- withr::local_tempfile(fileext = ".xlsx")
    openxlsx2::wb_load(example_inputs_path()) |>
      openxlsx2::wb_add_data("leave", "D99", dims = "A2") |>
      openxlsx2::wb_save(path)
    session$setInputs(file = list(datapath = path, name = "broken.xlsx"))
    expect_false(loaded()$ok)
    expect_true(any(grepl("D99", loaded()$problems$problem)))
  })
})

test_that("the results views and downloads work for a rota", {
  inputs <- read_rota_inputs(example_inputs_path())
  rota <- read_rota(test_path("fixtures", "example_rota_output.xlsx"))
  shiny::testServer(
    results_server,
    args = list(inputs = shiny::reactive(inputs), rota = shiny::reactive(rota),
                solution = shiny::reactive(NULL)),
    {
      expect_gt(sum(violations()$hard_soft == "hard"), 0)
      expect_equal(nrow(doctors()), 42)
      expect_true(file.exists(output$rota_xlsx))
      expect_equal(readxl::excel_sheets(output$rota_xlsx), c("by_role", "by_doctor", "doctors", "key", "long"))
      expect_true("doctors" %in% readxl::excel_sheets(output$report_xlsx))
      calendars <- zip::zip_list(output$calendars_zip)$filename
      expect_length(calendars, 43) # one per doctor, and one for everyone
      expect_true("All_doctors.ics" %in% calendars)
      expect_equal(read.csv(output$rota_csv, fileEncoding = "UTF-8-BOM")$doctor_id[1], "D01")
      expect_equal(nrow(read.csv(output$violations_csv, fileEncoding = "UTF-8-BOM")), nrow(violations()))
    }
  )
})

test_that("a solve runs in the background, reports progress and returns a compliant rota", {
  skip_on_cran()
  inputs <- two_weeks(read_rota_inputs(example_inputs_path()))
  loaded <- shiny::reactiveVal(list(inputs = inputs, problems = validate_inputs(inputs), ok = TRUE))
  shiny::testServer(solve_server, args = list(loaded = loaded), {
    session$setInputs(minutes = 20 / 60, seed = 1, solve = 1)
    expect_equal(state(), "running")
    waited <- 0
    while (state() %in% c("running", "stopping") && waited < 180) {
      Sys.sleep(1)
      waited <- waited + 1
      session$elapse(1000)
    }
    expect_equal(state(), "done")
    expect_gt(nrow(history()), 0)
    expect_null(solution()$model)
    v <- check_rota(solution()$rota, inputs)
    expect_equal(sum(v$hard_soft == "hard"), 0)
  })
})

test_that("the cell colours match the workbook styles", {
  colours <- code_colours(test_inputs()$shifts)
  night <- colours[colours$code == "ICU1_N", ]
  expect_equal(code_cell_style("ICU1_N", colours), list(background = night$fill, color = "#FFFFFF"))
  expect_equal(names(code_cell_style("ICU_DAY", colours)), "background")
  expect_null(code_cell_style(NA, colours))
  expect_equal(cell_style("unfilled"), list(background = "#F4C7C3", color = "#9C0006"))
  expect_null(cell_style(NA))
  expect_null(diff_style(0))
  expect_equal(diff_style(-4)$background, "rgb(90, 138, 198)")
})

test_that("the app opens a browser even when R has none set", {
  expect_equal(browser_command("firefox"), "firefox")
  local_mocked_bindings(Sys.which = \(names) c("xdg-open" = "/usr/bin/xdg-open", open = "")[names],
                        .package = "base")
  expect_equal(browser_command(""), "xdg-open")
  expect_equal(browser_command(NULL), "xdg-open")
})
