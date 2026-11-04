test_that("uk_bank_holidays lists each nation's bank holidays", {
  ew <- uk_bank_holidays("2027-01-01", "2027-12-31")
  expect_named(ew, c("date", "name"))
  expect_equal(nrow(ew), 8)
  expect_equal(ew$date[2], as.Date("2027-03-26"))
  expect_equal(ew$name[7], "Christmas Day (substitute)")
  expect_equal(uk_bank_holidays("2027-01-01", "2027-12-31", "england"), ew)

  scotland <- uk_bank_holidays("2027-01-01", "2027-12-31", "scotland")
  expect_true("St Andrew's Day" %in% scotland$name)
  expect_false("Easter Monday" %in% scotland$name)
  ni <- uk_bank_holidays("2027-01-01", "2027-12-31", "northern-ireland")
  expect_true("St Patrick's Day" %in% ni$name)
})

test_that("uk_bank_holidays keeps to the rota's dates and warns past the list's end", {
  march <- uk_bank_holidays(as.Date("2027-03-01"), as.Date("2027-03-28"))
  expect_equal(march$name, "Good Friday")
  expect_warning(uk_bank_holidays("2028-06-01", "2035-01-01"), "end on")
  expect_error(uk_bank_holidays("2027-01-01", "2027-12-31", "france"), "nation")
})

test_that("the example workbooks' bank holidays match GOV.UK's", {
  inputs <- test_inputs()
  expected <- uk_bank_holidays(inputs$period$start, inputs$period$end)
  expect_equal(as.Date(inputs$bank_holidays$date), expected$date)
})
