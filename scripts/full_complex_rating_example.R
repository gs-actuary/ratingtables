library(ratingtables)

# Full example: coverage-specific spec, interpolation, input_value, and custom function.
custom_discount <- function(row, coverage, current_premium, plan, spec_row, lookup) {
  if (as.character(row$paperless[[1]]) == "Y") 0.98 else 1.00
}

ft <- data.frame(
  state="IL", charter="STD", book_segment="new", rate_eff_date=as.Date("2025-01-01"), rate_exp_date=as.Date("2025-12-31"),
  coverage=c("HO","HO","HO","HO","HO","HO","HO"),
  term_name=c("base_rate","territory","territory","coverage_a_curve","coverage_a_curve","coverage_a_curve","expense_fee"),
  term_value=c(500,1.10,0.95,0.80,1.00,1.20,25),
  variable1=c(NA,"territory","territory","coverage_a","coverage_a","coverage_a",NA),
  level1=c(NA,"A","B","200000","300000","400000",NA),
  stringsAsFactors=FALSE
)
spec <- data.frame(
  coverage="HO", step_number=1:6,
  term_name=c("base_rate","scheduled_boats","territory","coverage_a_curve","paperless_discount","expense_fee"),
  value_source=c("factor_lookup","input_value","factor_lookup","interpolated_lookup","custom_function","factor_lookup"),
  calculation_type=c("multiplicative","additive","multiplicative","multiplicative","multiplicative","additive"),
  input_var=c(NA,"boat_premium",NA,"coverage_a",NA,NA), lookup_var=c(NA,NA,NA,"coverage_a",NA,NA),
  custom_function=c(NA,NA,NA,NA,"custom_discount",NA), stringsAsFactors=FALSE
)
plan <- new_rating_plan(ft, spec, coverages="HO", custom_functions=list(custom_discount=custom_discount))
pol <- data.frame(policy_id=c("P1","P2"), state="IL", charter="STD", book_segment="new", rating_date=as.Date("2025-06-01"), territory=c("A","B"), coverage_a=c(250000,350000), boat_premium=c(75,0), paperless=c("Y","N"), stringsAsFactors=FALSE)
res <- rate_policies_with_trace(pol, plan)
print(res$rated_data)
print(trace_to_excel_style(res$term_trace))
