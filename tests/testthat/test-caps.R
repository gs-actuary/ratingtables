test_that("caps apply when prior premium exists", {
  ex <- example_rating_plan()
  rated <- rate_policies(ex$policies, ex$plan)
  capped <- apply_caps(rated, coverages = c("BI", "PD"))

  expect_equal(capped$final_BI[1], rated$indicated_BI[1])
  expect_equal(capped$final_BI[2], 132) # prior 120, cap up 10%
  expect_equal(capped$final_BI[3], 165) # prior 150, cap up 10%
})

test_that("nearest dime rounding is available", {
  expect_equal(round_rating_value(12.34, "nearest_dime"), 12.3)
  expect_equal(round_rating_value(12.36, "nearest_dime"), 12.4)
})
