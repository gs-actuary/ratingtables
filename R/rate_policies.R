#' Rate a Policy Data Frame
#'
#' Rates every row of a policy data frame for each coverage in a rating plan.
#'
#' @param policies Policy-level input data frame.
#' @param plan A `rating_plan` object.
#'
#' @return The original policy data frame with `indicated_<coverage>` columns
#'   appended.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' rate_policies(ex$policies, ex$plan)
rate_policies <- function(policies, plan) {
  validate_policy_data(policies, plan)
  policies <- as.data.frame(policies, stringsAsFactors = FALSE)
  out <- policies

  for (cov in plan$coverages) {
    out[[paste0("indicated_", cov)]] <- NA_real_
  }

  for (row_i in seq_len(nrow(out))) {
    policy_row <- as.list(out[row_i, , drop = FALSE])
    for (cov in plan$coverages) {
      out[[paste0("indicated_", cov)]][[row_i]] <- rate_one_policy_coverage(
        policy_row = policy_row,
        plan = plan,
        coverage = cov,
        return_trace = FALSE
      )
    }
  }

  out
}
