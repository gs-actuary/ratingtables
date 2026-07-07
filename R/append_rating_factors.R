#' Append Wide Rating Factors to Rated Data
#'
#' Adds wide factor columns from a term trace to a rated policy data frame.
#'
#' @param rated_data Rated policy data frame.
#' @param term_trace Normalized term trace.
#' @param id_cols Columns used to join the wide factor table to `rated_data`.
#' @param value_col Trace column to pivot, usually `looked_up_value`.
#'
#' @return Rated data with wide factor columns appended.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' result <- rate_policies_with_trace(ex$policies, ex$plan)
#' append_rating_factors(result$rated_data, result$term_trace, id_cols = "policy_id")
append_rating_factors <- function(rated_data,
                                  term_trace,
                                  id_cols = c("row_id", "policy_id"),
                                  value_col = "looked_up_value") {
  rd <- as.data.frame(rated_data, stringsAsFactors = FALSE)
  tr <- as.data.frame(term_trace, stringsAsFactors = FALSE)
  id_cols <- id_cols[id_cols %in% names(rd) & id_cols %in% names(tr)]
  if (length(id_cols) == 0L) {
    stop("No shared id_cols found between rated_data and term_trace.", call. = FALSE)
  }
  wide <- trace_to_wide_factors(tr, id_cols = id_cols, value_col = value_col)
  .merge_preserve_order(rd, wide, by = id_cols, all_x = TRUE)
}
