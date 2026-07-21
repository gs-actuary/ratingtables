#' Rate policy records
#'
#' Apply a rating plan to every row and coverage in a policy-level data frame,
#' returning only the resulting rated data.
#'
#' @param rating_data A data frame containing one row per policy or rating
#'   record.
#' @param plan A `rating_plan` object created by [new_rating_plan()].
#' @param validate Logical. If `TRUE`, validate `rating_data` before rating.
#'
#' @return A data frame containing the original rating data plus one
#'   `indicated_<coverage>` column for each coverage in the rating plan.
#'
#' @export
rate_policies <- function(rating_data, plan, validate = TRUE) {
  rate_policies_with_trace(rating_data, plan, validate = validate)$rated_data
}

#' Rate policy records with trace output
#'
#' Apply a rating plan to every row and coverage in a policy-level data frame
#' and retain step-by-step trace information.
#'
#' @param rating_data A data frame containing one row per policy or rating
#'   record.
#' @param plan A `rating_plan` object created by [new_rating_plan()].
#' @param validate Logical. If `TRUE`, validate `rating_data` before rating.
#'
#' @return A `rating_result` object containing:
#' \describe{
#'   \item{rated_data}{The original records with indicated coverage values.}
#'   \item{term_trace}{Step-by-step rating trace rows.}
#'   \item{plan}{The rating plan used for the calculation.}
#' }
#'
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
