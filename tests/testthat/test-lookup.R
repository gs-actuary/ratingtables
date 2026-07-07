test_that("lookup returns one-way and constant terms", {
  ex <- example_rating_plan()
  expect_equal(lookup_term_value(ex$policies[1, ], ex$plan, "territory", "BI"), 100)
  expect_equal(lookup_term_value(ex$policies[1, ], ex$plan, "lat_lon_score", "BI"), 0.5)
})

test_that("lookup can return match metadata", {
  ex <- example_rating_plan()
  out <- lookup_term_value(ex$policies[1, ], ex$plan, "territory", "BI", return_match = TRUE)
  expect_equal(out$term_value, 100)
  expect_true("factor_row_id" %in% names(out))
  expect_equal(out$match_trace$variable, "territory")
})
