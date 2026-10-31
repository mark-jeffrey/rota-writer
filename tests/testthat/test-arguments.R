inputs_path <- function() system.file("extdata", "rota_inputs.xlsx", package = "rotawriter")
rota_path <- function() system.file("extdata", "example_rota_output.xlsx", package = "rotawriter")

test_that("check_rota accepts objects or file paths", {
  from_objects <- check_rota(read_rota(rota_path()), read_rota_inputs(inputs_path()))
  expect_equal(check_rota(rota_path(), inputs_path()), from_objects)
})

test_that("common mistakes give helpful errors", {
  inputs <- test_inputs()
  rota <- read_rota(rota_path())

  expect_error(check_rota(rota_path()), "needs both the rota and the inputs")
  expect_error(check_rota(inputs, rota), "Arguments are \\(rota, inputs\\)")
  expect_error(check_rota(rota, rota), "Arguments are \\(rota, inputs\\)")
  expect_error(check_rota(readxl::read_excel(rota_path(), "grid"), inputs), "not the \"grid\"")
  expect_error(read_rota(rota_path(), sheet = "grid"), "not the \"grid\"")
  expect_error(check_rota("no-such-rota.xlsx", inputs), "Can't find the rota workbook")
  expect_error(read_rota_inputs("no-such-inputs.xlsx"), "Can't find the inputs workbook")
})
