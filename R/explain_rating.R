#' Explain One Policy Rating
#'
#' Produces a compact, human-readable calculation trace for one policy and one
#' coverage.
#'
#' @param policy_row One-row data frame or named list.
#' @param plan A `rating_plan` object.
#' @param coverage Coverage to explain.
#'
#' @return A data frame with one row per rating step.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' explain_rating(ex$policies[1, ], ex$plan, "BI")
explain_rating <- function(policy_row, plan, coverage) {
  res <- rate_one_policy_coverage(
    policy_row = policy_row,
    plan = plan,
    coverage = coverage,
    return_trace = TRUE,
    trace_detail = "matches"
  )
  tr <- res$term_trace
  out <- tr[c("step_number", "term_name", "calculation_type", "looked_up_value", "continuous_var", "continuous_input", "contribution", "multiplier", "value_before_step", "value_after_step", "rate_set_key", "factor_row_id")]
  rownames(out) <- NULL
  out
}
