#' Build a Small Example Rating Plan
#'
#' Creates a small two-coverage rating plan and a small policy table. This is
#' used by examples, tests, and demos.
#'
#' @return A list with `plan` and `policies`.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' rate_policies(ex$policies, ex$plan)
example_rating_plan <- function() {
  max_vars <- 2L
  rate_meta <- data.frame(
    rate_set_key = "IL_HM_2025",
    state = "IL",
    charter = "HM",
    book_segment = "newbusiness",
    rate_eff_date = as.Date("2025-01-01"),
    rate_exp_date = as.Date("2025-12-31"),
    stringsAsFactors = FALSE
  )

  add_meta <- function(x) {
    cbind(rate_meta[rep(1L, nrow(x)), , drop = FALSE], x)
  }

  territory <- data.frame(
    coverage = c("BI", "BI", "PD", "PD"),
    term_name = "territory",
    territory = c("T1", "T2", "T1", "T2"),
    term_value = c(100, 125, 80, 100),
    stringsAsFactors = FALSE
  )
  territory <- populate_slots(add_meta(territory), c(territory = "territory"), max_vars = max_vars)

  credit <- data.frame(
    coverage = c("BI", "BI", "PD", "PD"),
    term_name = "credit",
    credit = c("A", "B", "A", "B"),
    term_value = c(0.90, 1.10, 0.95, 1.05),
    stringsAsFactors = FALSE
  )
  credit <- populate_slots(add_meta(credit), c(credit = "credit"), max_vars = max_vars)

  lat_lon <- data.frame(
    coverage = c("BI", "PD"),
    term_name = "lat_lon_score",
    term_value = c(0.50, 0.25),
    stringsAsFactors = FALSE
  )
  lat_lon <- make_empty_slots(add_meta(lat_lon), max_vars = max_vars)

  expense <- data.frame(
    coverage = c("BI", "BI", "PD", "PD"),
    term_name = "expense_fee",
    retention = c("good", "poor", "good", "poor"),
    term_value = c(20, 30, 15, 25),
    stringsAsFactors = FALSE
  )
  expense <- populate_slots(add_meta(expense), c(retention = "retention"), max_vars = max_vars)

  keep <- c("rate_set_key", "state", "charter", "book_segment", "rate_eff_date", "rate_exp_date", "coverage", "term_name", "term_value", .required_slot_cols(max_vars))
  factor_table <- rbind(territory[keep], credit[keep], lat_lon[keep], expense[keep])
  rownames(factor_table) <- NULL
  factor_table <- ensure_slot_columns(factor_table, max_vars = 12)

  rating_spec <- data.frame(
    term_name = c("territory", "credit", "lat_lon_score", "expense_fee"),
    calculation_type = c("multiplicative", "multiplicative", "continuous_additive", "additive"),
    continuous_var = c(NA, NA, "lat_lon_score", NA),
    rounding_rule = c(NA, NA, NA, "nearest_dollar"),
    rounding_digits = c(NA, NA, NA, NA),
    rounding_increment = c(NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )

  policies <- data.frame(
    policy_id = 1:3,
    state = "IL",
    charter = "HM",
    book_segment = "newbusiness",
    rating_date = as.Date("2025-06-15"),
    territory = c("T1", "T2", "T2"),
    credit = c("A", "A", "B"),
    lat_lon_score = c(2, 4, 6),
    retention = c("good", "good", "poor"),
    prior_premium_BI = c(NA, 120, 150),
    prior_premium_PD = c(NA, 90, 110),
    cap_up = c(NA, 0.10, 0.10),
    cap_down = c(NA, 0.05, 0.05),
    stringsAsFactors = FALSE
  )

  plan <- new_rating_plan(
    factor_table = factor_table,
    rating_spec = rating_spec,
    coverages = c("BI", "PD"),
    use_rate_set_key = FALSE,
    max_vars = max_vars,
    validate = TRUE
  )

  list(plan = plan, policies = policies)
}
