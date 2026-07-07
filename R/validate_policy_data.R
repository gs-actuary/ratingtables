#' Validate Policy Data for a Rating Plan
#'
#' Checks that a policy data frame contains the columns required by a rating
#' plan.
#'
#' @param policies Policy-level input data frame.
#' @param plan A `rating_plan` object.
#'
#' @return Invisibly returns `TRUE` if valid; otherwise errors.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' validate_policy_data(ex$policies, ex$plan)
validate_policy_data <- function(policies, plan) {
  if (!inherits(plan, "rating_plan")) {
    stop("plan must be a rating_plan object.", call. = FALSE)
  }
  policies <- as.data.frame(policies, stringsAsFactors = FALSE)
  required <- .required_policy_columns(plan)
  .stop_missing_cols(policies, required, "policies")

  if (!isTRUE(plan$use_rate_set_key) && !inherits(policies$rating_date, "Date")) {
    stop("policies$rating_date must be a Date column for automatic rate-set selection.", call. = FALSE)
  }

  invisible(TRUE)
}
