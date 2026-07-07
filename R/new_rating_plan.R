#' Create a Rating Plan
#'
#' Bundles a factor table, rating specification, coverages, and rating options
#' into a lightweight `rating_plan` object.
#'
#' @param factor_table Long-form factor table.
#' @param rating_spec Calculation specification. Required columns are
#'   `term_name`, `calculation_type`, and `continuous_var`. Optional rounding
#'   columns are `rounding_rule`, `rounding_digits`, and `rounding_increment`.
#' @param coverages Character vector of coverage names to rate.
#' @param use_rate_set_key If `TRUE`, rate-set selection uses `rate_set_key`
#'   from the policy data. If `FALSE`, automatic selection uses state, charter,
#'   book segment, and rating date.
#' @param max_vars Maximum number of variable/level slot pairs.
#' @param policy_id_col Optional policy identifier column used in trace output.
#' @param rounding_enabled If `FALSE`, rating ignores per-step rounding rules.
#' @param metadata Optional list of user metadata.
#' @param validate If `TRUE`, validate the plan during construction.
#'
#' @return An object of class `rating_plan`.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' print(ex$plan)
new_rating_plan <- function(factor_table,
                            rating_spec,
                            coverages,
                            use_rate_set_key = FALSE,
                            max_vars = 12L,
                            policy_id_col = "policy_id",
                            rounding_enabled = TRUE,
                            metadata = list(),
                            validate = TRUE) {

  max_vars <- as.integer(max_vars)

  factor_table <- ensure_slot_columns(
    factor_table = factor_table,
    max_vars = max_vars
  )

  rating_spec <- as.data.frame(rating_spec, stringsAsFactors = FALSE)

  plan <- list(
    factor_table = factor_table,
    rating_spec = rating_spec,
    coverages = as.character(coverages),
    use_rate_set_key = isTRUE(use_rate_set_key),
    max_vars = max_vars,
    policy_id_col = policy_id_col,
    rounding_enabled = isTRUE(rounding_enabled),
    metadata = metadata
  )

  class(plan) <- "rating_plan"

  if (isTRUE(validate)) {
    validate_rating_plan(plan)
  }

  plan
}
