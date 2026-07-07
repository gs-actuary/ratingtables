#' Summarize a Rating Plan
#'
#' Returns a compact data-frame summary of the rating specification by coverage
#' and term.
#'
#' @param object A `rating_plan` object.
#' @param ... Unused.
#'
#' @return A list with plan-level counts and a term summary.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' summary(ex$plan)
summary.rating_plan <- function(object, ...) {
  ft <- object$factor_table
  spec <- object$rating_spec
  term_summary <- data.frame(
    term_name = as.character(spec$term_name),
    calculation_type = as.character(spec$calculation_type),
    stringsAsFactors = FALSE
  )
  row_counts <- as.data.frame(table(ft$coverage, ft$term_name), stringsAsFactors = FALSE)
  names(row_counts) <- c("coverage", "term_name", "factor_rows")
  row_counts <- row_counts[row_counts$factor_rows > 0L, , drop = FALSE]
  list(
    coverages = object$coverages,
    factor_rows = nrow(ft),
    rating_terms = nrow(spec),
    rate_set_selection = if (isTRUE(object$use_rate_set_key)) "rate_set_key" else "automatic",
    term_summary = term_summary,
    factor_row_counts = row_counts
  )
}
