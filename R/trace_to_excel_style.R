#' Create an Excel-Style Rating Trace View
#'
#' Creates a wide, human-reviewable table containing rated policy outputs,
#' looked-up factors, and running values after each rating step.
#'
#' @param rating_result Result from `rate_policies_with_trace()`.
#' @param id_cols Columns used to join trace views to rated data.
#'
#' @return A wide data frame suitable for review. This function does not write
#'   an Excel file.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' result <- rate_policies_with_trace(ex$policies, ex$plan)
#' trace_to_excel_style(result)
trace_to_excel_style <- function(rating_result, id_cols = c("row_id", "policy_id")) {
  if (is.null(rating_result$rated_data) || is.null(rating_result$term_trace)) {
    stop("rating_result must contain rated_data and term_trace.", call. = FALSE)
  }
  rd <- rating_result$rated_data
  tr <- rating_result$term_trace
  id_cols <- id_cols[id_cols %in% names(rd) & id_cols %in% names(tr)]
  if (length(id_cols) == 0L) {
    if ("policy_id" %in% names(rd) && "policy_id" %in% names(tr)) {
      id_cols <- "policy_id"
    } else {
      stop("No usable id_cols found.", call. = FALSE)
    }
  }

  factors <- trace_to_wide_factors(tr, id_cols = id_cols, value_col = "looked_up_value")
  afters <- trace_to_wide_factors(tr, id_cols = id_cols, value_col = "value_after_step")
  names(afters)[!(names(afters) %in% id_cols)] <- paste0(names(afters)[!(names(afters) %in% id_cols)], "_after")

  out <- .merge_preserve_order(rd, factors, by = id_cols, all_x = TRUE)
  out <- .merge_preserve_order(out, afters, by = id_cols, all_x = TRUE)
  out
}
