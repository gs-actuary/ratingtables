#' Apply rate-change caps to indicated premiums
#'
#' Limit indicated premiums so that they do not increase or decrease by more
#' than specified percentages relative to prior premiums.
#'
#' @param rating_data A data frame containing indicated premium columns named
#'   `indicated_<coverage>`.
#' @param prior_data A data frame containing prior premium columns named
#'   `prior_<coverage>`.
#' @param by A character vector naming the column or columns used to join
#'   `rating_data` and `prior_data`.
#' @param coverages A character vector naming the coverages to cap.
#' @param max_increase An optional nonnegative numeric value giving the maximum
#'   permitted proportional increase. For example, `0.10` permits a 10 percent
#'   increase.
#' @param max_decrease An optional numeric value giving the maximum permitted
#'   proportional decrease. Its absolute value is used.
#'
#' @return A merged data frame with one `capped_<coverage>` column for each
#'   requested coverage.
#' @examples
#' indicated <- data.frame(
#'   policy_id = c("P1", "P2"),
#'   indicated_BI = c(125, 80)
#' )
#'
#' prior <- data.frame(
#'   policy_id = c("P1", "P2"),
#'   prior_BI = c(100, 100)
#' )
#'
#' apply_caps(
#'   rating_data = indicated,
#'   prior_data = prior,
#'   by = "policy_id",
#'   coverages = "BI",
#'   max_increase = 0.10,
#'   max_decrease = 0.15
#' )
#' @export
apply_caps <- function(rating_data, prior_data, by, coverages, max_increase = NULL, max_decrease = NULL) {
  d <- merge(as.data.frame(rating_data, stringsAsFactors = FALSE), as.data.frame(prior_data, stringsAsFactors = FALSE), by = by, all.x = TRUE, suffixes = c("", "_prior"), sort = FALSE)
  for (cov in coverages) {
    new_col <- paste0("indicated_", cov); prior_col <- paste0("prior_", cov); capped_col <- paste0("capped_", cov)
    .stop_missing_cols(d, c(new_col, prior_col), "rating_data/prior_data")
    low <- rep(-Inf, nrow(d)); high <- rep(Inf, nrow(d))
    if (!is.null(max_increase)) high <- d[[prior_col]] * (1 + max_increase)
    if (!is.null(max_decrease)) low <- d[[prior_col]] * (1 - abs(max_decrease))
    d[[capped_col]] <- pmin(pmax(d[[new_col]], low), high)
  }
  d
}

#' Split a rate set into review-friendly tables
#'
#' Separate a normalized long-form factor table into a collection of smaller
#' tables suitable for human review.
#'
#' @param factor_table A normalized long-form factor table.
#'
#' @return A named list of data frames containing review-friendly subsets of
#'   the supplied factor table.
#' @examples
#' ex <- example_rating_plan()
#'
#' tables <- rate_set_to_tables(
#'   ex$plan$factor_table
#' )
#'
#' names(tables)
#' tables$territory
#' @export
rate_set_to_tables <- function(factor_table) split(as.data.frame(factor_table, stringsAsFactors = FALSE), as.character(factor_table$term_name))
