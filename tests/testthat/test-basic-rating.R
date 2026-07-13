test_that("basic example rates", {
  ex <- example_rating_plan()
  res <- rate_policies_with_trace(ex$policies, ex$plan)
  expect_equal(round(res$rated_data$indicated_BI, 2), c(132, 108))
  expect_true(nrow(res$term_trace) == 6)
})

test_that("input_value terms are traced", {
  ft <- data.frame(state="IL", charter="STD", book_segment="new", rate_eff_date=as.Date("2025-01-01"), rate_exp_date=as.Date("2025-12-31"), coverage="BI", term_name="base", term_value=100, stringsAsFactors=FALSE)
  spec <- data.frame(step_number=1:2, term_name=c("base","external_factor"), value_source=c("factor_lookup","input_value"), calculation_type="multiplicative", input_var=c(NA,"external_factor"), stringsAsFactors=FALSE)
  plan <- new_rating_plan(ft, spec, "BI")
  d <- data.frame(policy_id="P1", state="IL", charter="STD", book_segment="new", rating_date=as.Date("2025-06-01"), external_factor=1.25)
  res <- rate_policies_with_trace(d, plan)
  expect_equal(res$rated_data$indicated_BI, 125)
  expect_true("input_value" %in% res$term_trace$value_source)
})

test_that("interpolation works", {
  ft <- data.frame(state="IL", charter="STD", book_segment="new", rate_eff_date=as.Date("2025-01-01"), rate_exp_date=as.Date("2025-12-31"), coverage="HO", term_name="curve", term_value=c(1,2), variable1="amount", level1=c("100","200"), stringsAsFactors=FALSE)
  spec <- data.frame(step_number=1, term_name="curve", value_source="interpolated_lookup", calculation_type="multiplicative", lookup_var="amount", stringsAsFactors=FALSE)
  plan <- new_rating_plan(ft, spec, "HO")
  d <- data.frame(policy_id="P1", state="IL", charter="STD", book_segment="new", rating_date=as.Date("2025-06-01"), amount=150)
  res <- rate_policies(d, plan)
  expect_equal(res$indicated_HO, 1.5)
})

test_that("entity aggregation supports mean and sum", {
  d <- data.frame(policy_id=c("P1","P1","P2"), indicated_BI=c(1,2,5), premium=c(10,20,30))
  m <- aggregate_entity_values(d, "policy_id", "indicated_BI", "mean", output_names="avg_BI")
  s <- aggregate_entity_values(d, "policy_id", "premium", "sum", output_names="sum_premium")
  expect_equal(m$avg_BI[m$policy_id=="P1"], 1.5)
  expect_equal(s$sum_premium[s$policy_id=="P1"], 30)
})
