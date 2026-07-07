#' Validate a Rating Plan
#'
#' Runs package-level validation on a `rating_plan` object.
#'
#' @param plan A `rating_plan` object.
#'
#' @return Invisibly returns `TRUE` if valid; otherwise errors.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' validate_rating_plan(ex$plan)
validate_rating_plan <- function(plan) {
  if (!inherits(plan, "rating_plan")) {
    stop("plan must be a rating_plan object.", call. = FALSE)
  }

  validate_factor_table(plan$factor_table, max_vars = plan$max_vars)
  validate_rating_spec(plan$rating_spec, plan$factor_table)
  validate_rate_sets(plan$factor_table, use_rate_set_key = plan$use_rate_set_key)

  missing_cov <- setdiff(plan$coverages, unique(as.character(plan$factor_table$coverage)))
  if (length(missing_cov) > 0L) {
    stop("Requested coverage(s) missing from factor_table: ", paste(missing_cov, collapse = ", "), call. = FALSE)
  }

  duplicate_rows <- find_duplicate_factors(
    plan$factor_table,
    max_vars = plan$max_vars,
    key_mode = if (isTRUE(plan$use_rate_set_key)) "rate_set_key" else "automatic"
  )
  if (nrow(duplicate_rows) > 0L) {
    stop("Duplicate factor rows found. Use find_duplicate_factors() to inspect them.", call. = FALSE)
  }

  for (cov in plan$coverages) {
    available_terms <- unique(as.character(plan$factor_table$term_name[plan$factor_table$coverage == cov]))
    missing_terms <- setdiff(as.character(plan$rating_spec$term_name), available_terms)
    if (length(missing_terms) > 0L) {
      stop("Coverage '", cov, "' is missing term(s): ", paste(missing_terms, collapse = ", "), call. = FALSE)
    }
  }

  invisible(TRUE)
}
