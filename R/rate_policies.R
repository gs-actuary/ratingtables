#' Rate policies and return rated data
#' @export
rate_policies <- function(rating_data, plan, validate = TRUE) {
  rate_policies_with_trace(rating_data, plan, validate = validate)$rated_data
}

#' Rate policies and return trace
#' @export
rate_policies_with_trace <- function(rating_data, plan, validate = TRUE) {
  if (!inherits(plan, "rating_plan")) stop("plan must be a rating_plan object.", call. = FALSE)
  d <- as.data.frame(rating_data, stringsAsFactors = FALSE)
  if (isTRUE(validate)) validate_policy_data(d, plan)
  out <- d
  trace_rows <- list(); k <- 1L
  for (i in seq_len(nrow(d))) {
    row <- d[i, , drop = FALSE]
    for (cov in plan$coverages) {
      ans <- rate_one_row_one_coverage(row, cov, plan, row_number = i)
      out[[paste0("indicated_", cov)]][i] <- ans$value
      trace_rows[[k]] <- ans$trace; k <- k + 1L
    }
  }
  res <- list(rated_data = out, term_trace = .combine_rows(trace_rows), plan = plan)
  class(res) <- "rating_result"
  res
}
