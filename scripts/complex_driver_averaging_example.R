# ============================================================
# Thorough driver-averaging rating example for ratingtables
# ============================================================
#
# This script is meant to be run from the package root after either:
#   devtools::load_all()
# or after installing/loading the package:
#   library(ratingtables)
#
# It demonstrates a fuller auto-style rating workflow using the rewritten
# ratingtables design:
#
#   1. One row per driver is rated with a driver-factor plan.
#   2. Driver factors are averaged by household.
#   3. The averaged driver factors are joined back to vehicle rows.
#   4. The vehicle/master ROC includes the averaged driver factor as a normal
#      input_value step, so it appears in the master vehicle trace.
#
# It includes the original prototype rating terms:
#   - territory
#   - credit
#   - gender
#   - marital_status
#   - driver_age
#   - underwriting_level
#   - driver_age x marital_status
#   - gender x driver_age
#   - lat_lon_score continuous additive term
#   - expense_fee
#   - capping after indicated premium
#
# It also includes two small examples that exercise the rewritten engine:
#   - an interpolated vehicle value curve
#   - a custom R function surcharge as an explicit escape hatch
#
# If you want a strict replica of the old prototype terms only, remove the
# vehicle_value_curve and custom_high_score_surcharge rows from vehicle_spec,
# and remove their corresponding factor-table/custom-function pieces.
# ============================================================

# devtools::load_all(".")
if (requireNamespace("ratingtables", quietly = TRUE)) {
  library(ratingtables)
}

required_functions <- c(
  "new_rating_plan", "rate_policies", "rate_policies_with_trace",
  "aggregate_entity_values", "join_entity_values", "apply_caps",
  "trace_to_excel_style", "trace_to_wide_factors", "explain_rating"
)
missing_functions <- required_functions[!vapply(required_functions, exists, logical(1), mode = "function")]
if (length(missing_functions) > 0) {
  stop(
    "ratingtables functions are not loaded. Run devtools::load_all('.') from the package root, ",
    "or install/load ratingtables. Missing: ", paste(missing_functions, collapse = ", "),
    call. = FALSE
  )
}

# -----------------------------
# Helpers used only in this script
# -----------------------------

max_vars <- 12L
coverages <- c("BI", "PD")

rate_sets <- data.frame(
  rate_set_key  = c("IL_HM_NB_2025A", "IL_HM_REN_2025A"),
  state         = c("IL", "IL"),
  charter       = c("HM", "HM"),
  book_segment  = c("newbusiness", "renewal"),
  rate_eff_date = as.Date(c("2025-01-01", "2025-01-01")),
  rate_exp_date = as.Date(c("2025-12-31", "2025-12-31")),
  stringsAsFactors = FALSE
)

empty_factor_row <- function() {
  out <- data.frame(
    rate_set_key = character(),
    state = character(),
    charter = character(),
    book_segment = character(),
    rate_eff_date = as.Date(character()),
    rate_exp_date = as.Date(character()),
    coverage = character(),
    term_name = character(),
    term_value = numeric(),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(max_vars)) {
    out[[paste0("variable", i)]] <- character()
    out[[paste0("level", i)]] <- character()
  }
  out
}

make_factor_rows <- function(term_name, coverage, term_value, vars = list(), rate_set = rate_sets[1, , drop = FALSE]) {
  n <- length(term_value)
  if (length(coverage) == 1) coverage <- rep(coverage, n)
  if (nrow(rate_set) == 1) rate_set <- rate_set[rep(1, n), , drop = FALSE]
  
  out <- data.frame(
    rate_set_key = as.character(rate_set$rate_set_key),
    state = as.character(rate_set$state),
    charter = as.character(rate_set$charter),
    book_segment = as.character(rate_set$book_segment),
    rate_eff_date = as.Date(rate_set$rate_eff_date),
    rate_exp_date = as.Date(rate_set$rate_exp_date),
    coverage = as.character(coverage),
    term_name = rep(term_name, n),
    term_value = as.numeric(term_value),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(max_vars)) {
    out[[paste0("variable", i)]] <- NA_character_
    out[[paste0("level", i)]] <- NA_character_
  }
  
  if (length(vars) > 0) {
    j <- 1L
    for (nm in names(vars)) {
      out[[paste0("variable", j)]] <- nm
      out[[paste0("level", j)]] <- as.character(vars[[nm]])
      j <- j + 1L
    }
  }
  
  out
}

add_for_both_rate_sets <- function(base_rows) {
  nb <- base_rows
  ren <- base_rows
  ren$rate_set_key <- "IL_HM_REN_2025A"
  ren$book_segment <- "renewal"
  ren$rate_eff_date <- as.Date("2025-01-01")
  ren$rate_exp_date <- as.Date("2025-12-31")
  
  # Apply modest renewal adjustments. These are intentionally simple so they
  # are easy to see in examples; they are not actuarial recommendations.
  mult_terms <- c(
    "credit", "gender", "marital_status", "driver_age",
    "underwriting_level", "driver_age_x_marital_status",
    "gender_x_driver_age", "vehicle_value_curve",
    "vehicle_usage", "territory"
  )
  additive_terms <- c("expense_fee")
  continuous_terms <- c("lat_lon_score")
  
  ren$term_value[ren$term_name %in% mult_terms] <- ren$term_value[ren$term_name %in% mult_terms] * 0.99
  ren$term_value[ren$term_name %in% additive_terms] <- ren$term_value[ren$term_name %in% additive_terms] + 2
  ren$term_value[ren$term_name %in% continuous_terms] <- ren$term_value[ren$term_name %in% continuous_terms] * 1.02
  
  rbind(nb, ren)
}

# -----------------------------
# 1. Driver-level factor table
# -----------------------------

build_driver_factor_table <- function() {
  rows <- list()
  k <- 1L
  
  for (cov in coverages) {
    # gender
    g <- data.frame(gender = c("M", "F"), stringsAsFactors = FALSE)
    val <- if (cov == "BI") c(1.08, 0.97) else c(1.06, 0.98)
    rows[[k]] <- make_factor_rows("gender", cov, val, list(gender = g$gender)); k <- k + 1L
    
    # marital_status
    m <- data.frame(marital_status = c("single", "married"), stringsAsFactors = FALSE)
    val <- if (cov == "BI") c(1.10, 0.95) else c(1.08, 0.96)
    rows[[k]] <- make_factor_rows("marital_status", cov, val, list(marital_status = m$marital_status)); k <- k + 1L
    
    # driver_age
    a <- data.frame(driver_age = c("18_24", "25_39", "40_64", "65_plus"), stringsAsFactors = FALSE)
    val <- if (cov == "BI") c(1.45, 1.10, 1.00, 1.08) else c(1.35, 1.08, 1.00, 1.06)
    rows[[k]] <- make_factor_rows("driver_age", cov, val, list(driver_age = a$driver_age)); k <- k + 1L
    
    # driver_age x marital_status
    grid <- expand.grid(
      driver_age = c("18_24", "25_39", "40_64", "65_plus"),
      marital_status = c("single", "married"),
      stringsAsFactors = FALSE
    )
    vals <- numeric(nrow(grid))
    for (i in seq_len(nrow(grid))) {
      da <- grid$driver_age[i]; ms <- grid$marital_status[i]
      vals[i] <- if (cov == "BI") {
        if (da == "18_24" && ms == "single") 1.12 else
          if (da == "18_24" && ms == "married") 1.05 else
            if (da == "25_39" && ms == "single") 1.05 else
              if (da == "25_39" && ms == "married") 0.98 else
                if (da == "40_64" && ms == "single") 1.02 else
                  if (da == "40_64" && ms == "married") 0.97 else
                    if (da == "65_plus" && ms == "single") 1.04 else 1.00
      } else {
        if (da == "18_24" && ms == "single") 1.10 else
          if (da == "18_24" && ms == "married") 1.04 else
            if (da == "25_39" && ms == "single") 1.04 else
              if (da == "25_39" && ms == "married") 0.99 else
                if (da == "40_64" && ms == "single") 1.01 else
                  if (da == "40_64" && ms == "married") 0.98 else
                    if (da == "65_plus" && ms == "single") 1.03 else 1.00
      }
    }
    rows[[k]] <- make_factor_rows(
      "driver_age_x_marital_status", cov, vals,
      list(driver_age = grid$driver_age, marital_status = grid$marital_status)
    ); k <- k + 1L
    
    # gender x driver_age
    grid <- expand.grid(
      gender = c("M", "F"),
      driver_age = c("18_24", "25_39", "40_64", "65_plus"),
      stringsAsFactors = FALSE
    )
    vals <- numeric(nrow(grid))
    for (i in seq_len(nrow(grid))) {
      gg <- grid$gender[i]; da <- grid$driver_age[i]
      vals[i] <- if (cov == "BI") {
        if (gg == "M" && da == "18_24") 1.07 else
          if (gg == "M" && da == "25_39") 1.03 else
            if (gg == "M" && da == "40_64") 1.00 else
              if (gg == "M" && da == "65_plus") 1.02 else
                if (gg == "F" && da == "18_24") 1.03 else
                  if (gg == "F" && da == "25_39") 0.99 else
                    if (gg == "F" && da == "40_64") 0.98 else 1.00
      } else {
        if (gg == "M" && da == "18_24") 1.05 else
          if (gg == "M" && da == "25_39") 1.02 else
            if (gg == "M" && da == "40_64") 1.00 else
              if (gg == "M" && da == "65_plus") 1.01 else
                if (gg == "F" && da == "18_24") 1.02 else
                  if (gg == "F" && da == "25_39") 0.99 else
                    if (gg == "F" && da == "40_64") 0.99 else 1.00
      }
    }
    rows[[k]] <- make_factor_rows(
      "gender_x_driver_age", cov, vals,
      list(gender = grid$gender, driver_age = grid$driver_age)
    ); k <- k + 1L
  }
  
  ft <- do.call(rbind, rows)
  ft <- add_for_both_rate_sets(ft)
  ft$factor_row_id <- seq_len(nrow(ft))
  ft
}

driver_factor_table <- build_driver_factor_table()

driver_spec <- data.frame(
  coverage = rep(coverages, each = 5),
  step_number = rep(1:5, times = length(coverages)),
  term_name = rep(c(
    "driver_age",
    "gender",
    "marital_status",
    "driver_age_x_marital_status",
    "gender_x_driver_age"
  ), times = length(coverages)),
  value_source = "factor_lookup",
  calculation_type = "multiplicative",
  input_var = NA_character_,
  stringsAsFactors = FALSE
)

driver_plan <- new_rating_plan(
  factor_table = driver_factor_table,
  rating_spec = driver_spec,
  coverages = coverages,
  use_rate_set_key = FALSE,
  max_vars = max_vars,
  policy_id_col = "driver_id",
  metadata = list(plan_id = "IL_HM_DRIVER_FACTOR_2025A"),
  validate = TRUE
)

# -----------------------------
# 2. Driver data: one row per driver
# -----------------------------

drivers <- data.frame(
  driver_id = paste0("D", 1:10),
  household_id = c("H1", "H1", "H2", "H2", "H2", "H3", "H4", "H4", "H5", "H5"),
  state = "IL",
  charter = "HM",
  rating_date = as.Date("2025-06-15"),
  book_segment = c(
    "newbusiness", "newbusiness",
    "renewal", "renewal", "renewal",
    "newbusiness",
    "renewal", "renewal",
    "newbusiness", "newbusiness"
  ),
  gender = c("M", "F", "F", "M", "M", "F", "M", "F", "M", "F"),
  marital_status = c("single", "single", "married", "married", "single", "married", "single", "single", "married", "married"),
  driver_age = c("18_24", "40_64", "40_64", "65_plus", "25_39", "25_39", "18_24", "25_39", "40_64", "65_plus"),
  stringsAsFactors = FALSE
)

driver_result <- rate_entities(drivers, driver_plan)
scored_drivers <- driver_result$rated_data

driver_avgs <- aggregate_entity_values(
  rated_entity_data = scored_drivers,
  group_col = "household_id",
  value_cols = c("indicated_BI", "indicated_PD"),
  aggregation = "mean",
  output_names = c("avg_driver_factor_BI", "avg_driver_factor_PD")
)

# -----------------------------
# 3. Vehicle/master factor table
# -----------------------------

build_vehicle_factor_table <- function() {
  rows <- list()
  k <- 1L
  
  for (cov in coverages) {
    # In the original prototype, territory served as the first multiplicative
    # term and effectively initialized the premium with a territory base rate.
    territory <- c("T1", "T2", "T3", "T4")
    val <- if (cov == "BI") c(120, 145, 170, 200) else c(90, 110, 130, 155)
    rows[[k]] <- make_factor_rows("territory", cov, val, list(territory = territory)); k <- k + 1L
    
    credit <- c("A", "B", "C", "D")
    val <- if (cov == "BI") c(0.85, 0.95, 1.05, 1.20) else c(0.88, 0.96, 1.04, 1.16)
    rows[[k]] <- make_factor_rows("credit", cov, val, list(credit = credit)); k <- k + 1L
    
    uw <- c("preferred", "standard", "nonstandard")
    val <- if (cov == "BI") c(0.92, 1.00, 1.22) else c(0.94, 1.00, 1.18)
    rows[[k]] <- make_factor_rows("underwriting_level", cov, val, list(underwriting_level = uw)); k <- k + 1L
    
    # Continuous additive coefficient. This multiplies the row's lat_lon_score
    # and adds the result to the running premium.
    val <- if (cov == "BI") 0.045 else 0.035
    rows[[k]] <- make_factor_rows("lat_lon_score", cov, val); k <- k + 1L
    
    # Expense fee varies by retention segment.
    retention <- c("good", "poor")
    val <- if (cov == "BI") c(20, 35) else c(15, 25)
    rows[[k]] <- make_factor_rows("expense_fee", cov, val, list(retention = retention)); k <- k + 1L
    
    # Interpolated vehicle value curve. This is not from the old prototype, but
    # it exercises the rewritten interpolated_lookup feature.
    vehicle_value <- c(10000, 15000, 20000, 25000, 30000, 40000)
    val <- if (cov == "BI") {
      c(0.98, 1.00, 1.03, 1.06, 1.09, 1.14)
    } else {
      c(0.96, 1.00, 1.05, 1.10, 1.15, 1.25)
    }
    rows[[k]] <- make_factor_rows("vehicle_value_curve", cov, val, list(vehicle_value = vehicle_value)); k <- k + 1L
  }
  
  ft <- do.call(rbind, rows)
  ft <- add_for_both_rate_sets(ft)
  ft$factor_row_id <- seq_len(nrow(ft))
  ft
}

vehicle_factor_table <- build_vehicle_factor_table()

# Custom function escape hatch. It has access to the row, coverage, current
# ROC-stage premium, the plan, the spec row, and a lookup helper. This example
# is intentionally simple: a high geospatial score and nonstandard underwriting
# create a modest multiplicative surcharge. It also calls lookup() just to show
# that factor lookups are available to custom functions.
custom_high_score_surcharge <- function(row, coverage, current_premium, plan, spec_row, lookup) {
  # A custom function can perform extra factor lookups if needed.
  # Here the result is not necessary for the formula, but demonstrates access.
  credit_lookup <- lookup("credit")
  
  score <- as.numeric(row$lat_lon_score[[1]])
  uw <- as.character(row$underwriting_level[[1]])
  
  surcharge <- if (score >= 7 && uw == "nonstandard") 1.03 else 1.00
  list(
    value = surcharge,
    trace = data.frame(
      detail = "custom_high_score_surcharge",
      credit_factor_available = credit_lookup$value,
      stringsAsFactors = FALSE
    )
  )
}

vehicle_spec <- data.frame(
  coverage = rep(coverages, each = 8),
  step_number = rep(1:8, times = length(coverages)),
  term_name = rep(c(
    "territory",
    "average_driver_factor",
    "credit",
    "underwriting_level",
    "vehicle_value_curve",
    "lat_lon_score",
    "custom_high_score_surcharge",
    "expense_fee"
  ), times = length(coverages)),
  value_source = rep(c(
    "factor_lookup",
    "input_value",
    "factor_lookup",
    "factor_lookup",
    "interpolated_lookup",
    "factor_lookup",
    "custom_function",
    "factor_lookup"
  ), times = length(coverages)),
  calculation_type = rep(c(
    "multiplicative",
    "multiplicative",
    "multiplicative",
    "multiplicative",
    "multiplicative",
    "continuous_additive",
    "multiplicative",
    "additive"
  ), times = length(coverages)),
  input_var = c(
    NA, "avg_driver_factor_BI", NA, NA, NA, "lat_lon_score", NA, NA,
    NA, "avg_driver_factor_PD", NA, NA, NA, "lat_lon_score", NA, NA
  ),
  lookup_var = rep(c(NA, NA, NA, NA, "vehicle_value", NA, NA, NA), times = length(coverages)),
  bounds = "error",
  custom_function = rep(c(NA, NA, NA, NA, NA, NA, "custom_high_score_surcharge", NA), times = length(coverages)),
  rounding_rule = rep(c(NA, NA, NA, NA, NA, NA, NA, "nearest_dime"), times = length(coverages)),
  stringsAsFactors = FALSE
)

vehicle_plan <- new_rating_plan(
  factor_table = vehicle_factor_table,
  rating_spec = vehicle_spec,
  coverages = coverages,
  use_rate_set_key = FALSE,
  max_vars = max_vars,
  policy_id_col = "vehicle_id",
  custom_functions = list(custom_high_score_surcharge = custom_high_score_surcharge),
  metadata = list(plan_id = "IL_HM_VEHICLE_MASTER_2025A"),
  validate = TRUE
)

# -----------------------------
# 4. Vehicle/master data
# -----------------------------

vehicles <- data.frame(
  vehicle_id = paste0("V", 1:8),
  policy_id = c("P1", "P1", "P2", "P2", "P3", "P4", "P5", "P5"),
  household_id = c("H1", "H1", "H2", "H2", "H3", "H4", "H5", "H5"),
  state = "IL",
  charter = "HM",
  rating_date = as.Date("2025-06-15"),
  book_segment = c(
    "newbusiness", "newbusiness", "renewal", "renewal",
    "newbusiness", "renewal", "newbusiness", "newbusiness"
  ),
  territory = c("T1", "T2", "T3", "T4", "T2", "T1", "T4", "T3"),
  credit = c("A", "B", "C", "D", "B", "A", "C", "D"),
  underwriting_level = c(
    "preferred", "standard", "nonstandard", "standard",
    "standard", "preferred", "nonstandard", "standard"
  ),
  lat_lon_score = c(3.2, 5.8, 7.5, 2.1, 6.9, 3.3, 8.0, 4.7),
  retention = c("good", "good", "poor", "poor", "poor", "good", "poor", "good"),
  vehicle_value = c(12500, 21750, 32000, 18000, 27500, 16000, 39000, 23500),
  stringsAsFactors = FALSE
)

vehicles_with_driver_avgs <- join_entity_values(vehicles, driver_avgs, by = "household_id")

vehicle_result <- rate_policies_with_trace(vehicles_with_driver_avgs, vehicle_plan)
rated_vehicles <- vehicle_result$rated_data

# -----------------------------
# 5. Capping and policy totals
# -----------------------------

prior_vehicle_premium <- data.frame(
  vehicle_id = paste0("V", 1:8),
  prior_BI = c(NA, NA, 340, 420, NA, 275, NA, NA),
  prior_PD = c(NA, NA, 260, 300, NA, 210, NA, NA),
  stringsAsFactors = FALSE
)

rated_vehicles_capped <- apply_caps(
  rating_data = rated_vehicles,
  prior_data = prior_vehicle_premium,
  by = "vehicle_id",
  coverages = coverages,
  max_increase = 0.15,
  max_decrease = 0.05
)

policy_totals <- aggregate(
  cbind(indicated_BI, indicated_PD) ~ policy_id,
  data = rated_vehicles,
  FUN = sum
)
names(policy_totals)[names(policy_totals) == "indicated_BI"] <- "policy_indicated_BI"
names(policy_totals)[names(policy_totals) == "indicated_PD"] <- "policy_indicated_PD"

# -----------------------------
# 6. Expose the rating and traces
# -----------------------------

cat("\n===== Scored drivers =====\n")
print(scored_drivers[, c(
  "driver_id", "household_id", "gender", "marital_status", "driver_age",
  "indicated_BI", "indicated_PD"
)], row.names = FALSE)

cat("\n===== Average driver factors by household =====\n")
print(driver_avgs, row.names = FALSE)

cat("\n===== Vehicles with averaged driver factors joined =====\n")
print(vehicles_with_driver_avgs[, c(
  "vehicle_id", "policy_id", "household_id", "territory", "credit",
  "underwriting_level", "lat_lon_score", "vehicle_value",
  "avg_driver_factor_BI", "avg_driver_factor_PD"
)], row.names = FALSE)

cat("\n===== Rated vehicle output =====\n")
print(rated_vehicles[, c(
  "vehicle_id", "policy_id", "household_id", "territory", "credit",
  "underwriting_level", "lat_lon_score", "vehicle_value",
  "avg_driver_factor_BI", "avg_driver_factor_PD",
  "indicated_BI", "indicated_PD"
)], row.names = FALSE)

cat("\n===== Capped vehicle output where prior premium exists =====\n")
print(rated_vehicles_capped[, c(
  "vehicle_id", "policy_id", "indicated_BI", "prior_BI", "capped_BI",
  "indicated_PD", "prior_PD", "capped_PD"
)], row.names = FALSE)

cat("\n===== Policy-level indicated totals =====\n")
print(policy_totals, row.names = FALSE)

cat("\n===== Master vehicle ROC trace for V1 / BI =====\n")
print(explain_rating(vehicle_result, row_number = 1, coverage = "BI"), row.names = FALSE)

cat("\n===== Driver source trace for D1 / BI =====\n")
print(explain_rating(driver_result, row_number = 1, coverage = "BI"), row.names = FALSE)

cat("\n===== Vehicle trace rows showing average driver factor, interpolation, and custom step =====\n")
interesting <- vehicle_result$term_trace[
  vehicle_result$term_trace$term_name %in% c("average_driver_factor", "vehicle_value_curve", "custom_high_score_surcharge"),
  c(
    "record_id", "coverage", "step_number", "term_name", "value_source",
    "calculation_type", "input_var", "input_value", "looked_up_value",
    "applied_value", "lower_level", "upper_level", "interpolation_weight",
    "value_before_step", "value_after_step", "custom_function"
  )
]
print(interesting, row.names = FALSE)

cat("\n===== Wide trace factors, first few rows =====\n")
print(head(trace_to_wide_factors(vehicle_result$term_trace)), row.names = FALSE)

# Objects left in the environment for interactive exploration:
#   driver_factor_table, driver_spec, drivers, driver_plan, driver_result,
#   scored_drivers, driver_avgs,
#   vehicle_factor_table, vehicle_spec, vehicles, vehicle_plan,
#   vehicles_with_driver_avgs, vehicle_result, rated_vehicles,
#   rated_vehicles_capped, policy_totals
# ============================================================