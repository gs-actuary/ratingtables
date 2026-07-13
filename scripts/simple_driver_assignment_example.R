library(ratingtables)

factor_table <- data.frame(
  state="IL", charter="STD", book_segment="new", rate_eff_date=as.Date("2025-01-01"), rate_exp_date=as.Date("2025-12-31"),
  coverage=c("BI","BI","BI","BI","BI","PD","PD","PD","PD"),
  term_name=c("base_rate","assigned_driver_age","assigned_driver_age","territory","territory","base_rate","assigned_driver_age","assigned_driver_age","symbol"),
  term_value=c(200,1.40,1.00,1.10,0.95,150,1.30,1.00,1.20),
  variable1=c(NA,"assigned_driver_age","assigned_driver_age","territory","territory",NA,"assigned_driver_age","assigned_driver_age","symbol"),
  level1=c(NA,"young","adult","A","B",NA,"young","adult","S1"),
  stringsAsFactors=FALSE
)

spec <- data.frame(
  coverage=c("BI","BI","BI","PD","PD","PD"),
  step_number=c(1,2,3,1,2,3),
  term_name=c("base_rate","assigned_driver_age","territory","base_rate","assigned_driver_age","symbol"),
  value_source="factor_lookup",
  calculation_type="multiplicative",
  stringsAsFactors=FALSE
)

policies <- data.frame(policy_id=c("V1","V2"), state="IL", charter="STD", book_segment="new", rating_date=as.Date("2025-06-01"), assigned_driver_age=c("young","adult"), territory=c("A","B"), symbol="S1", stringsAsFactors=FALSE)
plan <- new_rating_plan(factor_table, spec, coverages=c("BI","PD"))
res <- rate_policies_with_trace(policies, plan)
print(res$rated_data)
print(trace_to_excel_style(res$term_trace))
