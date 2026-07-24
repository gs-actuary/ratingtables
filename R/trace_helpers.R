#' Reshape rating trace to wide factor columns
#'
#' Convert normalized trace rows into one row per rating record and coverage,
#' with a separate column for each rating step.
#'
#' @param term_trace A data frame containing normalized rating trace rows,
#'   typically from [rate_policies_with_trace()].
#' @param id_cols A character vector naming the columns that uniquely identify
#'   each output row.
#'
#' @return A wide data frame containing the identifying columns and one
#'   `step_<number>_<term>` column for each rating step.
#' @examples
#' ex <- example_rating_plan()
#'
#' result <- rate_policies_with_trace(
#'   ex$policies,
#'   ex$plan
#' )
#'
#' trace_to_wide_factors(
#'   result$term_trace
#' )
#' @export
trace_to_wide_factors <- function(term_trace, id_cols = c("row_number", "record_id", "coverage")) {
  tr <- as.data.frame(term_trace, stringsAsFactors = FALSE)
  if (nrow(tr) == 0) return(tr)
  keep <- intersect(id_cols, names(tr))
  keys <- unique(tr[keep])
  for (i in seq_len(nrow(tr))) {
    nm <- paste0("step_", tr$step_number[i], "_", tr$term_name[i])
    idx <- rep(TRUE, nrow(keys))
    for (k in keep) idx <- idx & as.character(keys[[k]]) == as.character(tr[[k]][i])
    keys[[nm]][idx] <- tr$applied_value[i]
  }
  keys
}

#' Reshape rating trace to an Excel-style step table
#'
#' Select and order the principal trace columns to produce a compact,
#' human-readable view of the rating calculation.
#'
#' @param term_trace A data frame containing normalized rating trace rows,
#'   typically from [rate_policies_with_trace()].
#'
#' @return A data frame ordered by record, coverage, and step number, containing
#'   the principal rating inputs, applied values, and before-and-after values.
#' @examples
#' ex <- example_rating_plan()
#'
#' result <- rate_policies_with_trace(
#'   ex$policies,
#'   ex$plan
#' )
#'
#' trace_to_excel_style(
#'   result$term_trace
#' )
#' @export
trace_to_excel_style <- function(term_trace) {
  tr <- as.data.frame(term_trace, stringsAsFactors = FALSE)
  if (nrow(tr) == 0) return(tr)
  cols <- c("row_number", "record_id", "coverage", "step_number", "term_name", "value_source", "calculation_type", "applied_value", "value_before_step", "value_after_step")
  tr[order(tr$row_number, tr$coverage, tr$step_number), intersect(cols, names(tr)), drop = FALSE]
}

#' Append rating factors to rated data
#'
#' Reshape normalized trace rows to wide rating-factor columns and join them
#' back to the rated records.
#'
#' @param rated_data A data frame containing rated policy or entity records.
#' @param term_trace A data frame containing normalized rating trace rows.
#' @param by A character vector naming the identifying column or columns used
#'   to join the wide trace values to `rated_data`.
#'
#' @return A data frame containing the rated records with wide rating-step
#'   columns appended.
#' @examples
#' ex <- example_rating_plan()
#'
#' result <- rate_policies_with_trace(
#'   ex$policies,
#'   ex$plan
#' )
#'
#' append_rating_factors(
#'   rated_data = result$rated_data,
#'   term_trace = result$term_trace,
#'   by = "row_number"
#' )
#' @export
append_rating_factors <- function(rated_data, term_trace, by = "row_number") {
  d <- as.data.frame(rated_data, stringsAsFactors = FALSE); d$row_number <- seq_len(nrow(d))
  merge(d, trace_to_wide_factors(term_trace, id_cols = by), by = by, all.x = TRUE, sort = FALSE)
}

#' Explain a rating calculation
#'
#' Extract and format the rating trace for one source-data row and, optionally,
#' one coverage.
#'
#' @param rating_result A `rating_result` object returned by
#'   [rate_policies_with_trace()].
#' @param row_number An integer identifying the source-data row to explain.
#' @param coverage An optional character string identifying the coverage to
#'   include. If `NULL`, all coverages for the row are returned.
#'
#' @return An Excel-style data frame showing the selected rating steps in
#'   calculation order.
#' @examples
#' ex <- example_rating_plan()
#'
#' result <- rate_policies_with_trace(
#'   ex$policies,
#'   ex$plan
#' )
#'
#' explain_rating(
#'   rating_result = result,
#'   row_number = 1,
#'   coverage = "BI"
#' )
#' @export
explain_rating <- function(rating_result, row_number = 1, coverage = NULL) {
  tr <- rating_result$term_trace
  out <- tr[tr$row_number == row_number, , drop = FALSE]
  if (!is.null(coverage)) out <- out[out$coverage == coverage, , drop = FALSE]
  trace_to_excel_style(out)
}
