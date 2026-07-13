#' Reshape trace to wide applied values
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

#' Reshape trace to an Excel-style step table
#' @export
trace_to_excel_style <- function(term_trace) {
  tr <- as.data.frame(term_trace, stringsAsFactors = FALSE)
  if (nrow(tr) == 0) return(tr)
  cols <- c("row_number", "record_id", "coverage", "step_number", "term_name", "value_source", "calculation_type", "applied_value", "value_before_step", "value_after_step")
  tr[order(tr$row_number, tr$coverage, tr$step_number), intersect(cols, names(tr)), drop = FALSE]
}

#' Append wide rating factors to rated data
#' @export
append_rating_factors <- function(rated_data, term_trace, by = "row_number") {
  d <- as.data.frame(rated_data, stringsAsFactors = FALSE); d$row_number <- seq_len(nrow(d))
  merge(d, trace_to_wide_factors(term_trace, id_cols = by), by = by, all.x = TRUE, sort = FALSE)
}

#' Explain one rated row
#' @export
explain_rating <- function(rating_result, row_number = 1, coverage = NULL) {
  tr <- rating_result$term_trace
  out <- tr[tr$row_number == row_number, , drop = FALSE]
  if (!is.null(coverage)) out <- out[out$coverage == coverage, , drop = FALSE]
  trace_to_excel_style(out)
}
