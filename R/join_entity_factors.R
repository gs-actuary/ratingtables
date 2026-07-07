#' Join Entity Factors Back to Policy Rows
#'
#' Joins group-level averaged factors back to policy, vehicle, or other rating
#' rows.
#'
#' @param rating_data Data frame to receive averaged factors.
#' @param entity_factor_data Data frame produced by `average_entity_factors()`.
#' @param by Join column name.
#'
#' @return `rating_data` with factor columns joined.
#' @export
#'
#' @examples
#' rating_data <- data.frame(policy_id = 1:2, household_id = c("H1", "H2"))
#' factors <- data.frame(household_id = c("H1", "H2"), avg_factor_BI = c(1.0, 1.2))
#' join_entity_factors(rating_data, factors, by = "household_id")
join_entity_factors <- function(rating_data, entity_factor_data, by) {
  .merge_preserve_order(
    as.data.frame(rating_data, stringsAsFactors = FALSE),
    as.data.frame(entity_factor_data, stringsAsFactors = FALSE),
    by = by,
    all_x = TRUE
  )
}
