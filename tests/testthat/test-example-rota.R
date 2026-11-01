test_that("every planted violation in the example rota is detected", {
  path <- system.file("extdata", "example_rota_output.xlsx", package = "rotawriter")
  inputs <- read_rota_inputs(system.file("extdata", "rota_inputs.xlsx", package = "rotawriter"))
  v <- check_rota(read_rota(path), inputs)
  known <- readxl::read_excel(path, "known_violations")

  for (i in seq_len(nrow(known))) {
    k <- known[i, ]
    found <- v |>
      dplyr::filter(rule_id == k$violation, doctor_id == k$doctor_id,
                    start_date <= as.Date(k$last_date), end_date >= as.Date(k$first_date))
    expect(nrow(found) > 0, paste("Not detected:", k$violation, k$doctor_id))
  }
})

test_that("an empty rota fails cover for every required slot", {
  inputs <- test_inputs()
  v <- check_rota(rota_of(character(), as.Date(character()), character()), inputs)
  required <- expected_cover(inputs) |> dplyr::filter(n_min > 0)

  expect_equal(sum(v$rule_id == "COVER"), nrow(required))
  expect_true(all(is.na(v$doctor_id[v$rule_id == "COVER"])))
})
