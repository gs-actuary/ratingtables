#' Create a rating plan
#' @param factor_table Normalized long-format factor table.
#' @param rating_spec Rating specification.
#' @param coverages Character vector of coverages to rate.
#' @param use_rate_set_key Whether rating rows select factors using rate_set_key.
#' @param max_vars Maximum number of variable/level slots.
#' @param policy_id_col Policy identifier column.
#' @param custom_functions Named list of user-written R functions.
#' @param validate Whether to validate.
#' @param metadata Optional metadata list.
#' @examples
#' ex <- example_rating_plan()
#'
#' plan <- new_rating_plan(
#'   factor_table = ex$plan$factor_table,
#'   rating_spec = ex$plan$rating_spec,
#'   coverages = "BI"
#' )
#'
#' print(plan)
#' summary(plan)
#' @export
new_rating_plan <- function(factor_table, rating_spec, coverages, use_rate_set_key = FALSE, max_vars = 12, policy_id_col = "policy_id", custom_functions = list(), validate = TRUE, metadata = list()) {
  max_vars <- .normalize_max_vars(max_vars)
  ft <- ensure_slot_columns(as.data.frame(factor_table, stringsAsFactors = FALSE), max_vars)
  if (!("factor_row_id" %in% names(ft))) ft$factor_row_id <- seq_len(nrow(ft))
  spec <- .normalize_rating_spec(rating_spec)
  plan <- list(factor_table = ft, rating_spec = spec, coverages = as.character(coverages), use_rate_set_key = isTRUE(use_rate_set_key), max_vars = max_vars, policy_id_col = policy_id_col, custom_functions = custom_functions, metadata = metadata)
  class(plan) <- "rating_plan"
  if (isTRUE(validate)) validate_rating_plan(plan)
  plan
}


#' @export
print.rating_plan <- function(x, ...) {
  cat("<rating_plan>\n")
  cat("  coverages:", paste(x$coverages, collapse = ", "), "\n")
  cat("  factor rows:", nrow(x$factor_table), "\n")
  cat("  spec rows:", nrow(x$rating_spec), "\n")
  invisible(x)
}

#' @export
summary.rating_plan <- function(object, ...) {
  out <- list(coverages = object$coverages, factor_rows = nrow(object$factor_table), spec_rows = nrow(object$rating_spec), factor_terms = sort(unique(as.character(object$factor_table$term_name))), spec_terms = sort(unique(as.character(object$rating_spec$term_name))))
  class(out) <- "summary.rating_plan"
  out
}
