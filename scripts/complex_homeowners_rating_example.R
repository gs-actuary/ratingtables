# Homeowners peril rating example for ratingtables
#
# Demonstrates:
#   - Coverage/peril-specific ROCs: FIRE, WIND_HAIL, WATER, OTHER
#   - Common homeowners rating variables:
#       deductible, Coverage A, credit tier, territory, age of home
#   - Native interpolation for Coverage A curve factors
#   - Scheduled item and boat endorsement premiums as sub-entity calculations
#   - Generalized entity aggregation: boats and scheduled items both use
#       rate_entities() -> aggregate_entity_values() -> join_entity_values()
#   - Master homeowners ROC includes entity-derived premiums as input_value
#       steps, so they appear in the main policy trace.
#
# Assumes the rewritten ratingtables package is installed or loaded via devtools::load_all().

library(ratingtables)

# -----------------------------------------------------------------------------
# Utility for examples
# -----------------------------------------------------------------------------

blank_to_zero <- function(x) {
  x[is.na(x)] <- 0
  x
}

# -----------------------------------------------------------------------------
# Parent homeowners policy data
# -----------------------------------------------------------------------------

home_policies <- data.frame(
  policy_id = c("H001", "H002", "H003"),
  state = "IL",
  charter = "STD",
  book_segment = "new",
  rating_date = as.Date("2025-06-15"),
  
  coverage_a = c(275000, 420000, 335000),
  deductible = c("1000", "2500", "500"),
  credit_tier = c("A", "B", "C"),
  territory = c("T1", "T2", "T3"),
  age_home_band = c("newer", "mature", "older"),
  
  stringsAsFactors = FALSE
)

# The base pure-premium terms below are written as rate per $1,000 of Coverage A.
home_policies$coverage_a_thousands <- home_policies$coverage_a / 1000

# -----------------------------------------------------------------------------
# Boat sub-entities
# -----------------------------------------------------------------------------

boats <- data.frame(
  policy_id = c("H001", "H001", "H003"),
  boat_id = c("B001", "B002", "B003"),
  state = "IL",
  charter = "STD",
  book_segment = "new",
  rating_date = as.Date("2025-06-15"),
  
  boat_coverage = c(12000, 8000, 18000),
  boat_class = c("small_power", "sail", "small_power"),
  
  stringsAsFactors = FALSE
)

boat_factor_table <- data.frame(
  state = "IL",
  charter = "STD",
  book_segment = "new",
  rate_eff_date = as.Date("2025-01-01"),
  rate_exp_date = as.Date("2025-12-31"),
  coverage = "BOAT",
  term_name = "boat_rate_per_dollar",
  term_value = c(0.010, 0.007),
  variable1 = "boat_class",
  level1 = c("small_power", "sail"),
  stringsAsFactors = FALSE
)

boat_spec <- data.frame(
  coverage = "BOAT",
  step_number = 1,
  term_name = "boat_rate_per_dollar",
  value_source = "factor_lookup",
  calculation_type = "continuous_additive",
  input_var = "boat_coverage",
  stringsAsFactors = FALSE
)

boat_plan <- new_rating_plan(
  factor_table = boat_factor_table,
  rating_spec = boat_spec,
  coverages = "BOAT"
)

boat_result <- rate_entities(boats, boat_plan)

boat_by_policy <- aggregate_entity_values(
  rated_entity_data = boat_result$rated_data,
  group_col = "policy_id",
  value_cols = "indicated_BOAT",
  aggregation = "sum",
  output_names = "boat_premium"
)

# -----------------------------------------------------------------------------
# Scheduled item sub-entities
# -----------------------------------------------------------------------------

scheduled_items <- data.frame(
  policy_id = c("H001", "H002", "H002", "H003"),
  item_id = c("S001", "S002", "S003", "S004"),
  state = "IL",
  charter = "STD",
  book_segment = "new",
  rating_date = as.Date("2025-06-15"),
  
  item_type = c("jewelry", "fine_art", "guns", "jewelry"),
  scheduled_coverage = c(15000, 25000, 6000, 10000),
  
  stringsAsFactors = FALSE
)

scheduled_factor_table <- data.frame(
  state = "IL",
  charter = "STD",
  book_segment = "new",
  rate_eff_date = as.Date("2025-01-01"),
  rate_exp_date = as.Date("2025-12-31"),
  coverage = "SCHEDULED_ITEM",
  term_name = "scheduled_item_rate_per_dollar",
  term_value = c(0.012, 0.006, 0.009),
  variable1 = "item_type",
  level1 = c("jewelry", "fine_art", "guns"),
  stringsAsFactors = FALSE
)

scheduled_spec <- data.frame(
  coverage = "SCHEDULED_ITEM",
  step_number = 1,
  term_name = "scheduled_item_rate_per_dollar",
  value_source = "factor_lookup",
  calculation_type = "continuous_additive",
  input_var = "scheduled_coverage",
  stringsAsFactors = FALSE
)

scheduled_plan <- new_rating_plan(
  factor_table = scheduled_factor_table,
  rating_spec = scheduled_spec,
  coverages = "SCHEDULED_ITEM"
)

scheduled_result <- rate_entities(scheduled_items, scheduled_plan)

scheduled_by_policy <- aggregate_entity_values(
  rated_entity_data = scheduled_result$rated_data,
  group_col = "policy_id",
  value_cols = "indicated_SCHEDULED_ITEM",
  aggregation = "sum",
  output_names = "scheduled_item_premium"
)

# -----------------------------------------------------------------------------
# Join entity premiums to the master homeowners rows
# -----------------------------------------------------------------------------

home_with_entities <- join_entity_values(home_policies, boat_by_policy, by = "policy_id")
home_with_entities <- join_entity_values(home_with_entities, scheduled_by_policy, by = "policy_id")

home_with_entities$boat_premium <- blank_to_zero(home_with_entities$boat_premium)
home_with_entities$scheduled_item_premium <- blank_to_zero(home_with_entities$scheduled_item_premium)

# -----------------------------------------------------------------------------
# Homeowners factor table
# -----------------------------------------------------------------------------

perils <- c("FIRE", "WIND_HAIL", "WATER", "OTHER")

# Base rates are per $1,000 of Coverage A.
base_rate_rows <- data.frame(
  state = "IL",
  charter = "STD",
  book_segment = "new",
  rate_eff_date = as.Date("2025-01-01"),
  rate_exp_date = as.Date("2025-12-31"),
  coverage = perils,
  term_name = "base_rate_per_1000_cov_a",
  term_value = c(0.70, 1.15, 0.85, 0.40),
  variable1 = NA_character_,
  level1 = NA_character_,
  stringsAsFactors = FALSE
)

# Coverage A curve: shown as interpolation points. In a real manual, these might
# be every $5,000; this example uses coarse points for readability.
coverage_a_points <- c(200000, 300000, 400000, 500000)
curve_by_peril <- list(
  FIRE      = c(0.92, 1.00, 1.06, 1.11),
  WIND_HAIL = c(0.90, 1.00, 1.08, 1.15),
  WATER     = c(0.95, 1.00, 1.04, 1.08),
  OTHER     = c(0.96, 1.00, 1.03, 1.06)
)
coverage_a_curve_rows <- do.call(rbind, lapply(perils, function(cov) {
  data.frame(
    state = "IL",
    charter = "STD",
    book_segment = "new",
    rate_eff_date = as.Date("2025-01-01"),
    rate_exp_date = as.Date("2025-12-31"),
    coverage = cov,
    term_name = "coverage_a_curve",
    term_value = curve_by_peril[[cov]],
    variable1 = "coverage_a",
    level1 = as.character(coverage_a_points),
    stringsAsFactors = FALSE
  )
}))

# Deductible factors differ by peril.
deductible_factor_values <- data.frame(
  deductible = c("500", "1000", "2500", "5000"),
  FIRE      = c(1.08, 1.00, 0.92, 0.86),
  WIND_HAIL = c(1.15, 1.00, 0.84, 0.72),
  WATER     = c(1.12, 1.00, 0.88, 0.80),
  OTHER     = c(1.05, 1.00, 0.95, 0.90),
  stringsAsFactors = FALSE
)
deductible_rows <- do.call(rbind, lapply(perils, function(cov) {
  data.frame(
    state = "IL",
    charter = "STD",
    book_segment = "new",
    rate_eff_date = as.Date("2025-01-01"),
    rate_exp_date = as.Date("2025-12-31"),
    coverage = cov,
    term_name = "deductible",
    term_value = deductible_factor_values[[cov]],
    variable1 = "deductible",
    level1 = deductible_factor_values$deductible,
    stringsAsFactors = FALSE
  )
}))

credit_rows <- do.call(rbind, lapply(perils, function(cov) {
  mult <- switch(cov, FIRE = 1.00, WIND_HAIL = 0.70, WATER = 1.10, OTHER = 0.90)
  data.frame(
    state = "IL",
    charter = "STD",
    book_segment = "new",
    rate_eff_date = as.Date("2025-01-01"),
    rate_exp_date = as.Date("2025-12-31"),
    coverage = cov,
    term_name = "credit_tier",
    term_value = 1 + mult * c(-0.08, 0.00, 0.12),
    variable1 = "credit_tier",
    level1 = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
}))

territory_values <- data.frame(
  territory = c("T1", "T2", "T3"),
  FIRE      = c(0.96, 1.03, 1.10),
  WIND_HAIL = c(0.88, 1.10, 1.28),
  WATER     = c(0.95, 1.04, 1.12),
  OTHER     = c(0.98, 1.02, 1.08),
  stringsAsFactors = FALSE
)
territory_rows <- do.call(rbind, lapply(perils, function(cov) {
  data.frame(
    state = "IL",
    charter = "STD",
    book_segment = "new",
    rate_eff_date = as.Date("2025-01-01"),
    rate_exp_date = as.Date("2025-12-31"),
    coverage = cov,
    term_name = "territory",
    term_value = territory_values[[cov]],
    variable1 = "territory",
    level1 = territory_values$territory,
    stringsAsFactors = FALSE
  )
}))

age_values <- data.frame(
  age_home_band = c("newer", "mature", "older"),
  FIRE      = c(0.92, 1.00, 1.14),
  WIND_HAIL = c(0.98, 1.00, 1.05),
  WATER     = c(0.88, 1.00, 1.20),
  OTHER     = c(0.95, 1.00, 1.08),
  stringsAsFactors = FALSE
)
age_rows <- do.call(rbind, lapply(perils, function(cov) {
  data.frame(
    state = "IL",
    charter = "STD",
    book_segment = "new",
    rate_eff_date = as.Date("2025-01-01"),
    rate_exp_date = as.Date("2025-12-31"),
    coverage = cov,
    term_name = "age_home",
    term_value = age_values[[cov]],
    variable1 = "age_home_band",
    level1 = age_values$age_home_band,
    stringsAsFactors = FALSE
  )
}))

home_factor_table <- rbind(
  base_rate_rows,
  coverage_a_curve_rows,
  deductible_rows,
  credit_rows,
  territory_rows,
  age_rows
)

# -----------------------------------------------------------------------------
# Homeowners coverage/peril-specific ROC spec
# -----------------------------------------------------------------------------

make_peril_spec <- function(cov) {
  data.frame(
    coverage = cov,
    step_number = 1:6,
    term_name = c(
      "base_rate_per_1000_cov_a",
      "coverage_a_curve",
      "deductible",
      "credit_tier",
      "territory",
      "age_home"
    ),
    value_source = c(
      "factor_lookup",
      "interpolated_lookup",
      "factor_lookup",
      "factor_lookup",
      "factor_lookup",
      "factor_lookup"
    ),
    calculation_type = c(
      "continuous_additive",
      "multiplicative",
      "multiplicative",
      "multiplicative",
      "multiplicative",
      "multiplicative"
    ),
    input_var = c("coverage_a_thousands", "coverage_a", NA, NA, NA, NA),
    lookup_var = c(NA, "coverage_a", NA, NA, NA, NA),
    bounds = c("error", "error", "error", "error", "error", "error"),
    stringsAsFactors = FALSE
  )
}

home_spec <- do.call(rbind, lapply(perils, make_peril_spec))

# Boats and scheduled items are endorsement premiums. In this example they are
# attached to the OTHER peril/coverage, and they appear as normal ROC steps in
# the master homeowners trace.
home_spec <- rbind(
  home_spec,
  data.frame(
    coverage = "OTHER",
    step_number = c(7, 8),
    term_name = c("scheduled_items", "boats"),
    value_source = c("input_value", "input_value"),
    calculation_type = c("additive", "additive"),
    input_var = c("scheduled_item_premium", "boat_premium"),
    lookup_var = c(NA, NA),
    bounds = c("error", "error"),
    stringsAsFactors = FALSE
  )
)

home_plan <- new_rating_plan(
  factor_table = home_factor_table,
  rating_spec = home_spec,
  coverages = perils
)

home_result <- rate_policies_with_trace(home_with_entities, home_plan)

# -----------------------------------------------------------------------------
# Policy totals and outputs
# -----------------------------------------------------------------------------

rated_home <- home_result$rated_data
rated_home$total_homeowners_premium <- rowSums(
  rated_home[paste0("indicated_", perils)],
  na.rm = TRUE
)

cat("\nBoat entity results:\n")
print(boat_result$rated_data[, c("policy_id", "boat_id", "boat_class", "boat_coverage", "indicated_BOAT")])

cat("\nScheduled item entity results:\n")
print(scheduled_result$rated_data[, c("policy_id", "item_id", "item_type", "scheduled_coverage", "indicated_SCHEDULED_ITEM")])

cat("\nAggregated entity premiums joined to master policies:\n")
print(home_with_entities[, c("policy_id", "boat_premium", "scheduled_item_premium")])

cat("\nHomeowners peril premiums and total premium:\n")
print(rated_home[, c(
  "policy_id",
  "coverage_a",
  "deductible",
  "credit_tier",
  "territory",
  "age_home_band",
  paste0("indicated_", perils),
  "total_homeowners_premium"
)])

cat("\nMaster ROC trace, first policy only:\n")
print(home_result$term_trace[home_result$term_trace$record_id == "H001", ])

cat("\nExcel-style trace, first few rows:\n")
print(head(trace_to_excel_style(home_result$term_trace), 20))

# Optional examples of the entity aggregation pattern:
#   Boats:           aggregation = "sum" over indicated_BOAT
#   Scheduled items: aggregation = "sum" over indicated_SCHEDULED_ITEM
# A driver averaging workflow would use the same aggregate_entity_values()
# function with aggregation = "mean" over driver factor outputs.