test_that("trace helper output is wide", {
  ex <- example_rating_plan()
  result <- rate_policies_with_trace(ex$policies, ex$plan)
  wide <- trace_to_wide_factors(result$term_trace)
  expect_true("BI_territory" %in% names(wide))
  expect_equal(nrow(wide), nrow(ex$policies))
})

test_that("excel-style trace can be created", {
  ex <- example_rating_plan()
  result <- rate_policies_with_trace(ex$policies, ex$plan)
  wide <- trace_to_excel_style(result)
  expect_true("BI_territory" %in% names(wide))
  expect_true("BI_territory_after" %in% names(wide))
})
