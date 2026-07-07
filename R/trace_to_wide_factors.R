#' Pivot Term Trace to Wide Factor Columns
#'
#' Converts a normalized term trace into one row per ID combination with one
#' factor column per coverage/term.
#'
#' @param term_trace A normalized term trace, usually from
#'   `rate_policies_with_trace()`.
#' @param id_cols Columns identifying output rows.
#' @param value_col Trace column to pivot, usually `looked_up_value`.
#' @param include_coverage_in_name If `TRUE`, output columns are named like
#'   `BI_territory`; otherwise they are named by term.
#'
#' @return A wide data frame.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' result <- rate_policies_with_trace(ex$policies, ex$plan)
#' trace_to_wide_factors(result$term_trace)
trace_to_wide_factors <- function(term_trace,
                                  id_cols = c("row_id", "policy_id"),
                                  value_col = "looked_up_value",
                                  include_coverage_in_name = TRUE) {
  tr <- as.data.frame(term_trace, stringsAsFactors = FALSE)
  .stop_missing_cols(tr, c(id_cols, "coverage", "term_name", value_col), "term_trace")

  ids <- unique(tr[id_cols])
  out <- ids

  for (i in seq_len(nrow(tr))) {
    nm <- if (isTRUE(include_coverage_in_name)) {
      paste(tr$coverage[[i]], tr$term_name[[i]], sep = "_")
    } else {
      as.character(tr$term_name[[i]])
    }
    if (!(nm %in% names(out))) {
      out[[nm]] <- NA_real_
    }
    idx <- rep(TRUE, nrow(out))
    for (col in id_cols) {
      idx <- idx & out[[col]] == tr[[col]][[i]]
    }
    out[[nm]][idx] <- tr[[value_col]][[i]]
  }

  rownames(out) <- NULL
  out
}
