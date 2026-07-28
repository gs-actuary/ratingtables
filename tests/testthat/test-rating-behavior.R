test_that("exact lookup prefers the most specific matching factor", {
  factor_table <- data.frame(
    coverage = c("BI", "BI"),
    term_name = c("territory", "territory"),
    term_value = c(1.00, 1.20),
    variable1 = c(NA, "territory"),
    level1 = c(NA, "A"),
    stringsAsFactors = FALSE
  )
  
  rating_spec <- data.frame(
    step_number = 1,
    term_name = "territory",
    value_source = "factor_lookup",
    calculation_type = "multiplicative",
    stringsAsFactors = FALSE
  )
  
  plan <- new_rating_plan(
    factor_table = factor_table,
    rating_spec = rating_spec,
    coverages = "BI"
  )
  
  territory_a <- data.frame(territory = "A")
  territory_b <- data.frame(territory = "B")
  
  result_a <- lookup_exact_value(
    row = territory_a,
    coverage = "BI",
    plan = plan,
    term_name = "territory"
  )
  
  result_b <- lookup_exact_value(
    row = territory_b,
    coverage = "BI",
    plan = plan,
    term_name = "territory"
  )
  
  expect_equal(result_a$value, 1.20)
  expect_equal(result_b$value, 1.00)
})


test_that("exact lookup rejects ambiguous and missing matches", {
  ambiguous_table <- data.frame(
    coverage = c("BI", "BI"),
    term_name = c("territory", "territory"),
    term_value = c(1.20, 1.25),
    variable1 = c("territory", "territory"),
    level1 = c("A", "A"),
    stringsAsFactors = FALSE
  )
  
  rating_spec <- data.frame(
    step_number = 1,
    term_name = "territory",
    value_source = "factor_lookup",
    calculation_type = "multiplicative",
    stringsAsFactors = FALSE
  )
  
  ambiguous_plan <- new_rating_plan(
    factor_table = ambiguous_table,
    rating_spec = rating_spec,
    coverages = "BI"
  )
  
  expect_error(
    lookup_exact_value(
      row = data.frame(territory = "A"),
      coverage = "BI",
      plan = ambiguous_plan,
      term_name = "territory"
    ),
    "Ambiguous factor lookup"
  )
  
  specific_table <- ambiguous_table[1, , drop = FALSE]
  
  specific_plan <- new_rating_plan(
    factor_table = specific_table,
    rating_spec = rating_spec,
    coverages = "BI"
  )
  
  expect_error(
    lookup_exact_value(
      row = data.frame(territory = "B"),
      coverage = "BI",
      plan = specific_plan,
      term_name = "territory"
    ),
    "No matching factor row"
  )
})


test_that("interpolation handles exact points and boundary rules", {
  factor_table <- data.frame(
    coverage = c("HO", "HO"),
    term_name = c("curve", "curve"),
    term_value = c(1.00, 2.00),
    variable1 = c("amount", "amount"),
    level1 = c("100", "200"),
    stringsAsFactors = FALSE
  )
  
  rating_spec <- data.frame(
    step_number = 1,
    term_name = "curve",
    value_source = "interpolated_lookup",
    calculation_type = "multiplicative",
    lookup_var = "amount",
    stringsAsFactors = FALSE
  )
  
  plan <- new_rating_plan(
    factor_table = factor_table,
    rating_spec = rating_spec,
    coverages = "HO"
  )
  
  exact_result <- lookup_interpolated_value(
    row = data.frame(amount = 100),
    coverage = "HO",
    plan = plan,
    term_name = "curve",
    lookup_var = "amount"
  )
  
  expect_equal(exact_result$value, 1.00)
  expect_equal(exact_result$lower_level, 100)
  expect_equal(exact_result$upper_level, 100)
  expect_equal(exact_result$interpolation_weight, 0)
  
  expect_error(
    lookup_interpolated_value(
      row = data.frame(amount = 250),
      coverage = "HO",
      plan = plan,
      term_name = "curve",
      lookup_var = "amount",
      bounds = "error"
    ),
    "outside table bounds"
  )
  
  clamped_result <- lookup_interpolated_value(
    row = data.frame(amount = 250),
    coverage = "HO",
    plan = plan,
    term_name = "curve",
    lookup_var = "amount",
    bounds = "clamp"
  )
  
  extrapolated_result <- lookup_interpolated_value(
    row = data.frame(amount = 250),
    coverage = "HO",
    plan = plan,
    term_name = "curve",
    lookup_var = "amount",
    bounds = "extrapolate"
  )
  
  expect_equal(clamped_result$value, 2.00)
  expect_equal(extrapolated_result$value, 2.50)
})


test_that("interpolation rejects duplicate curve levels", {
  factor_table <- data.frame(
    coverage = c("HO", "HO", "HO"),
    term_name = c("curve", "curve", "curve"),
    term_value = c(1.00, 2.00, 2.10),
    variable1 = c("amount", "amount", "amount"),
    level1 = c("100", "200", "200"),
    stringsAsFactors = FALSE
  )
  
  rating_spec <- data.frame(
    step_number = 1,
    term_name = "curve",
    value_source = "interpolated_lookup",
    calculation_type = "multiplicative",
    lookup_var = "amount",
    stringsAsFactors = FALSE
  )
  
  plan <- new_rating_plan(
    factor_table = factor_table,
    rating_spec = rating_spec,
    coverages = "HO"
  )
  
  expect_error(
    lookup_interpolated_value(
      row = data.frame(amount = 150),
      coverage = "HO",
      plan = plan,
      term_name = "curve",
      lookup_var = "amount"
    ),
    "Duplicate interpolation x-values"
  )
})


test_that("coverage-specific specs use different rating orders", {
  factor_table <- data.frame(
    coverage = c("BI", "PD", "PD"),
    term_name = c("bi_base", "pd_base", "pd_fee"),
    term_value = c(100, 50, 10),
    stringsAsFactors = FALSE
  )
  
  rating_spec <- data.frame(
    coverage = c("BI", "PD", "PD"),
    step_number = c(1, 1, 2),
    term_name = c("bi_base", "pd_base", "pd_fee"),
    value_source = "factor_lookup",
    calculation_type = c(
      "multiplicative",
      "multiplicative",
      "additive"
    ),
    stringsAsFactors = FALSE
  )
  
  plan <- new_rating_plan(
    factor_table = factor_table,
    rating_spec = rating_spec,
    coverages = c("BI", "PD")
  )
  
  policies <- data.frame(
    policy_id = "P1",
    stringsAsFactors = FALSE
  )
  
  result <- rate_policies_with_trace(
    rating_data = policies,
    plan = plan
  )
  
  expect_equal(result$rated_data$indicated_BI, 100)
  expect_equal(result$rated_data$indicated_PD, 60)
  
  bi_trace <- result$term_trace[
    result$term_trace$coverage == "BI",
    ,
    drop = FALSE
  ]
  
  pd_trace <- result$term_trace[
    result$term_trace$coverage == "PD",
    ,
    drop = FALSE
  ]
  
  expect_equal(as.character(bi_trace$term_name), "bi_base")
  expect_equal(
    as.character(pd_trace$term_name),
    c("pd_base", "pd_fee")
  )
})


test_that("calculation types update the running value correctly", {
  factor_table <- data.frame(
    coverage = rep("BI", 4),
    term_name = c(
      "base",
      "per_unit_charge",
      "exposure_slope",
      "replacement"
    ),
    term_value = c(
      100,
      2,
      0.10,
      50.06
    ),
    stringsAsFactors = FALSE
  )
  
  rating_spec <- data.frame(
    step_number = 1:5,
    term_name = c(
      "base",
      "fee",
      "per_unit_charge",
      "exposure_slope",
      "replacement"
    ),
    value_source = c(
      "factor_lookup",
      "input_value",
      "factor_lookup",
      "factor_lookup",
      "factor_lookup"
    ),
    calculation_type = c(
      "multiplicative",
      "additive",
      "continuous_additive",
      "continuous_multiplicative",
      "replace"
    ),
    input_var = c(
      NA,
      "fee",
      "units",
      "exposure",
      NA
    ),
    rounding_rule = c(
      NA,
      NA,
      NA,
      NA,
      "nearest_dime"
    ),
    stringsAsFactors = FALSE
  )
  
  plan <- new_rating_plan(
    factor_table = factor_table,
    rating_spec = rating_spec,
    coverages = "BI"
  )
  
  policy <- data.frame(
    policy_id = "P1",
    fee = 10,
    units = 3,
    exposure = 2,
    stringsAsFactors = FALSE
  )
  
  result <- rate_policies_with_trace(
    rating_data = policy,
    plan = plan
  )
  
  # Running calculation:
  # 100
  # 100 + 10 = 110
  # 110 + 2 * 3 = 116
  # 116 * (1 + 0.10 * 2) = 139.2
  # replace with 50.06, then round to nearest dime = 50.1
  
  expect_equal(
    result$term_trace$value_after_step,
    c(100, 110, 116, 139.2, 50.1),
    tolerance = 1e-10
  )
  
  expect_equal(
    result$rated_data$indicated_BI,
    50.1,
    tolerance = 1e-10
  )
})


test_that("custom functions can use current premium and lookup helper", {
  factor_table <- data.frame(
    coverage = c("BI", "BI"),
    term_name = c("base", "hidden_modifier"),
    term_value = c(100, 1.20),
    stringsAsFactors = FALSE
  )
  
  rating_spec <- data.frame(
    step_number = 1:2,
    term_name = c("base", "custom_total"),
    value_source = c("factor_lookup", "custom_function"),
    calculation_type = c("multiplicative", "custom"),
    custom_function = c(NA, "add_fee_after_modifier"),
    input_vars = c(NA, "fee"),
    stringsAsFactors = FALSE
  )
  
  add_fee_after_modifier <- function(
    row,
    coverage,
    current_premium,
    plan,
    spec_row,
    lookup
  ) {
    modifier <- lookup("hidden_modifier")$value
    
    current_premium * modifier + row$fee[[1]]
  }
  
  plan <- new_rating_plan(
    factor_table = factor_table,
    rating_spec = rating_spec,
    coverages = "BI",
    custom_functions = list(
      add_fee_after_modifier = add_fee_after_modifier
    )
  )
  
  policy <- data.frame(
    policy_id = "P1",
    fee = 5,
    stringsAsFactors = FALSE
  )
  
  result <- rate_policies_with_trace(
    rating_data = policy,
    plan = plan
  )
  
  expect_equal(result$rated_data$indicated_BI, 125)
  
  expect_equal(
    as.character(result$term_trace$custom_function[2]),
    "add_fee_after_modifier"
  )
  
  expect_equal(result$term_trace$value_before_step[2], 100)
  expect_equal(result$term_trace$value_after_step[2], 125)
})


test_that("automatic rate-set selection uses the rating date", {
  factor_table <- data.frame(
    state = c("IL", "IL"),
    charter = c("STD", "STD"),
    book_segment = c("new", "new"),
    rate_eff_date = as.Date(c("2025-01-01", "2026-01-01")),
    rate_exp_date = as.Date(c("2025-12-31", "2026-12-31")),
    coverage = c("BI", "BI"),
    term_name = c("base", "base"),
    term_value = c(100, 120),
    stringsAsFactors = FALSE
  )
  
  rating_spec <- data.frame(
    step_number = 1,
    term_name = "base",
    value_source = "factor_lookup",
    calculation_type = "multiplicative",
    stringsAsFactors = FALSE
  )
  
  plan <- new_rating_plan(
    factor_table = factor_table,
    rating_spec = rating_spec,
    coverages = "BI"
  )
  
  policies <- data.frame(
    policy_id = c("P1", "P2"),
    state = "IL",
    charter = "STD",
    book_segment = "new",
    rating_date = as.Date(c("2025-06-01", "2026-06-01")),
    stringsAsFactors = FALSE
  )
  
  result <- rate_policies(
    rating_data = policies,
    plan = plan
  )
  
  expect_equal(
    result$indicated_BI,
    c(100, 120)
  )
})


test_that("validation catches malformed inputs", {
  bad_source_spec <- data.frame(
    term_name = "base",
    value_source = "unknown_source",
    calculation_type = "multiplicative",
    stringsAsFactors = FALSE
  )
  
  expect_error(
    validate_rating_spec(bad_source_spec),
    "Unsupported value_source"
  )
  
  missing_input_spec <- data.frame(
    term_name = "external_factor",
    value_source = "input_value",
    calculation_type = "multiplicative",
    stringsAsFactors = FALSE
  )
  
  expect_error(
    validate_rating_spec(missing_input_spec),
    "input_value rows require input_var"
  )
  
  bad_factor_table <- data.frame(
    term_name = "base",
    term_value = "not numeric",
    stringsAsFactors = FALSE
  )
  
  expect_error(
    validate_factor_table(bad_factor_table),
    "term_value must be numeric"
  )
  
  example <- example_rating_plan()
  
  incomplete_policies <- example$policies
  incomplete_policies$territory <- NULL
  
  expect_error(
    validate_policy_data(
      rating_data = incomplete_policies,
      plan = example$plan
    ),
    "territory"
  )
})


test_that("duplicate detection returns every conflicting row", {
  factor_table <- data.frame(
    coverage = c("BI", "BI", "BI"),
    term_name = c("territory", "territory", "territory"),
    term_value = c(1.10, 1.15, 0.95),
    variable1 = c("territory", "territory", "territory"),
    level1 = c("A", "A", "B"),
    stringsAsFactors = FALSE
  )
  
  duplicates <- find_duplicate_factors(
    factor_table,
    max_vars = 1
  )
  
  expect_equal(nrow(duplicates), 2)
  expect_equal(sort(duplicates$term_value), c(1.10, 1.15))
  expect_true(all(duplicates$level1 == "A"))
})


test_that("premium caps enforce increase and decrease limits", {
  indicated <- data.frame(
    policy_id = c("P1", "P2", "P3"),
    indicated_BI = c(125, 80, 103),
    stringsAsFactors = FALSE
  )
  
  prior <- data.frame(
    policy_id = c("P1", "P2", "P3"),
    prior_BI = c(100, 100, 100),
    stringsAsFactors = FALSE
  )
  
  result <- apply_caps(
    rating_data = indicated,
    prior_data = prior,
    by = "policy_id",
    coverages = "BI",
    max_increase = 0.10,
    max_decrease = 0.15
  )
  
  result <- result[
    match(c("P1", "P2", "P3"), result$policy_id),
    ,
    drop = FALSE
  ]
  
  expect_equal(
    result$capped_BI,
    c(110, 85, 103)
  )
})