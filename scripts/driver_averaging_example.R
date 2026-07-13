library(ratingtables)

driver_ft <- data.frame(
  state="IL", charter="STD", book_segment="new", rate_eff_date=as.Date("2025-01-01"), rate_exp_date=as.Date("2025-12-31"),
  coverage=c("BI","BI","BI","BI","PD","PD","PD","PD"),
  term_name=c("driver_age","driver_age","gender","gender","driver_age","driver_age","gender","gender"),
  term_value=c(1.40,1.00,1.08,0.97,1.30,1.00,1.05,0.98),
  variable1=c("driver_age","driver_age","gender","gender","driver_age","driver_age","gender","gender"),
  level1=c("young","adult","M","F","young","adult","M","F"),
  stringsAsFactors=FALSE
)
driver_spec <- data.frame(coverage=rep(c("BI","PD"), each=2), step_number=c(1,2,1,2), term_name=c("driver_age","gender","driver_age","gender"), value_source="factor_lookup", calculation_type="multiplicative", stringsAsFactors=FALSE)
driver_plan <- new_rating_plan(driver_ft, driver_spec, c("BI","PD"), policy_id_col="driver_id")
drivers <- data.frame(driver_id=c("D1","D2","D3"), household_id=c("H1","H1","H2"), state="IL", charter="STD", book_segment="new", rating_date=as.Date("2025-06-01"), driver_age=c("young","adult","adult"), gender=c("M","F","M"), stringsAsFactors=FALSE)
driver_result <- rate_entities(drivers, driver_plan)
avg <- aggregate_entity_values(driver_result$rated_data, group_col="household_id", value_cols=c("indicated_BI","indicated_PD"), aggregation="mean", output_names=c("avg_driver_factor_BI","avg_driver_factor_PD"))

vehicle_ft <- data.frame(
  state="IL", charter="STD", book_segment="new", rate_eff_date=as.Date("2025-01-01"), rate_exp_date=as.Date("2025-12-31"),
  coverage=c("BI","BI","BI","PD","PD","PD"),
  term_name=c("base_rate","territory","territory","base_rate","symbol","symbol"),
  term_value=c(200,1.10,0.95,150,1.20,0.90),
  variable1=c(NA,"territory","territory",NA,"symbol","symbol"), level1=c(NA,"A","B",NA,"S1","S2"), stringsAsFactors=FALSE)
vehicle_spec <- data.frame(
  coverage=c("BI","BI","BI","PD","PD","PD"), step_number=c(1,2,3,1,2,3),
  term_name=c("base_rate","avg_driver_factor","territory","base_rate","avg_driver_factor","symbol"),
  value_source=c("factor_lookup","input_value","factor_lookup","factor_lookup","input_value","factor_lookup"),
  calculation_type="multiplicative",
  input_var=c(NA,"avg_driver_factor_BI",NA,NA,"avg_driver_factor_PD",NA), stringsAsFactors=FALSE)
vehicle_plan <- new_rating_plan(vehicle_ft, vehicle_spec, c("BI","PD"), policy_id_col="vehicle_id")
vehicles <- data.frame(vehicle_id=c("V1","V2"), household_id=c("H1","H2"), state="IL", charter="STD", book_segment="new", rating_date=as.Date("2025-06-01"), territory=c("A","B"), symbol=c("S1","S2"), stringsAsFactors=FALSE)
vehicles2 <- join_entity_values(vehicles, avg, by="household_id")
vehicle_result <- rate_policies_with_trace(vehicles2, vehicle_plan)
print(driver_result$rated_data)
print(avg)
print(vehicle_result$rated_data)
print(trace_to_excel_style(vehicle_result$term_trace))
