#' Build a small example rating plan
#' @export
example_rating_plan <- function() {
  ft <- data.frame(
    state = "IL", charter = "STD", book_segment = "new", rate_eff_date = as.Date("2025-01-01"), rate_exp_date = as.Date("2025-12-31"),
    coverage = c("BI", "BI", "BI", "BI"),
    term_name = c("base_rate", "territory", "territory", "limit"),
    term_value = c(100, 1.1, 0.9, 1.2),
    variable1 = c(NA, "territory", "territory", "limit"),
    level1 = c(NA, "A", "B", "100/300"),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(step_number = 1:3, term_name = c("base_rate", "territory", "limit"), value_source = "factor_lookup", calculation_type = "multiplicative", stringsAsFactors = FALSE)
  policies <- data.frame(policy_id = c("P1", "P2"), state = "IL", charter = "STD", book_segment = "new", rating_date = as.Date("2025-06-01"), territory = c("A", "B"), limit = "100/300", stringsAsFactors = FALSE)
  list(plan = new_rating_plan(ft, spec, coverages = "BI"), policies = policies)
}
