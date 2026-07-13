#' Apply rate caps to indicated premiums
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
#' @export
rate_set_to_tables <- function(factor_table) split(as.data.frame(factor_table, stringsAsFactors = FALSE), as.character(factor_table$term_name))
