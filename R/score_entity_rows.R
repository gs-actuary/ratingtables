#' Score Entity Rows
#'
#' Rates entity-level rows, such as drivers, using a rating plan. The resulting
#' `indicated_<coverage>` columns can be interpreted as composite entity
#' factors when the plan is constructed that way.
#'
#' @param entity_data Entity-level data frame.
#' @param plan A `rating_plan` object.
#'
#' @return Rated entity data.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' score_entity_rows(ex$policies, ex$plan)
score_entity_rows <- function(entity_data, plan) {
  rate_policies(entity_data, plan)
}
