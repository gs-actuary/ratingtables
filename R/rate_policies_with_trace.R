#' Rate Policies With Trace Output
#'
#' Rates policies and returns the rated data plus normalized trace tables.
#'
#' @param policies Policy-level input data frame.
#' @param plan A `rating_plan` object.
#' @param trace_detail One of `"terms"`, `"matches"`, or `"all"`.
#' @param include_inputs If `TRUE`, include an `input_snapshot` table in the
#'   returned object.
#'
#' @return A list of class `rating_result` with at least `rated_data` and
#'   `term_trace`. With `trace_detail = "matches"` or `"all"`, also includes
#'   `factor_match_trace`. With `include_inputs = TRUE` or `trace_detail = "all"`,
#'   also includes `input_snapshot`.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' result <- rate_policies_with_trace(ex$policies, ex$plan, trace_detail = "matches")
#' result$rated_data
#' result$term_trace
rate_policies_with_trace <- function(policies,
                                     plan,
                                     trace_detail = c("terms", "matches", "all"),
                                     include_inputs = FALSE) {
  trace_detail <- match.arg(trace_detail)
  validate_policy_data(policies, plan)
  policies <- as.data.frame(policies, stringsAsFactors = FALSE)
  rated <- policies

  for (cov in plan$coverages) {
    rated[[paste0("indicated_", cov)]] <- NA_real_
  }

  term_traces <- list()
  match_traces <- list()

  for (row_i in seq_len(nrow(rated))) {
    policy_row <- as.list(rated[row_i, , drop = FALSE])
    policy_id <- .safe_policy_id(rated, row_i, plan$policy_id_col)

    for (cov in plan$coverages) {
      res <- rate_one_policy_coverage(
        policy_row = policy_row,
        plan = plan,
        coverage = cov,
        return_trace = TRUE,
        trace_detail = trace_detail,
        row_id = row_i,
        policy_id = policy_id
      )
      rated[[paste0("indicated_", cov)]][[row_i]] <- res$final_value
      term_traces[[length(term_traces) + 1L]] <- res$term_trace
      if (!is.null(res$factor_match_trace) && nrow(res$factor_match_trace) > 0L) {
        match_traces[[length(match_traces) + 1L]] <- res$factor_match_trace
      }
    }
  }

  term_trace <- if (length(term_traces) > 0L) do.call(rbind, term_traces) else data.frame()
  if (nrow(term_trace) > 0L) rownames(term_trace) <- NULL

  out <- list(rated_data = rated, term_trace = term_trace)

  if (trace_detail %in% c("matches", "all")) {
    factor_match_trace <- if (length(match_traces) > 0L) do.call(rbind, match_traces) else data.frame()
    if (nrow(factor_match_trace) > 0L) rownames(factor_match_trace) <- NULL
    out$factor_match_trace <- factor_match_trace
  }

  if (isTRUE(include_inputs) || trace_detail == "all") {
    input_snapshot <- policies
    input_snapshot$row_id <- seq_len(nrow(input_snapshot))
    out$input_snapshot <- input_snapshot
  }

  class(out) <- "rating_result"
  out
}
