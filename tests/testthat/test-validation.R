test_that("rating plan validates", {
  ex <- example_rating_plan()
  expect_true(validate_rating_plan(ex$plan))
  expect_true(validate_policy_data(ex$policies, ex$plan))
})

test_that("duplicate factors are found", {
  ex <- example_rating_plan()
  ft <- rbind(ex$plan$factor_table, ex$plan$factor_table[1, ])
  dups <- find_duplicate_factors(ft, max_vars = ex$plan$max_vars, key_mode = "automatic")
  expect_gt(nrow(dups), 0)
})

test_that("old continuous calculation type errors", {
  ex <- example_rating_plan()
  spec <- ex$plan$rating_spec
  spec$calculation_type[1] <- "continuous"
  expect_error(validate_rating_spec(spec, ex$plan$factor_table), "continuous_additive")
})
