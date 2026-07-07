#' Print a Rating Plan
#'
#' Prints a compact summary of a `rating_plan` object.
#'
#' @param x A `rating_plan` object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' print(ex$plan)
print.rating_plan <- function(x, ...) {
  cat("Rating plan\n")
  cat("- Coverages: ", paste(x$coverages, collapse = ", "), "\n", sep = "")
  cat("- Terms: ", paste(unique(as.character(x$rating_spec$term_name)), collapse = ", "), "\n", sep = "")
  cat("- Factor rows: ", nrow(x$factor_table), "\n", sep = "")
  cat("- Max interaction order: ", x$max_vars, "\n", sep = "")
  cat("- Rate-set selection: ", if (isTRUE(x$use_rate_set_key)) "rate_set_key" else "automatic", "\n", sep = "")
  invisible(x)
}
