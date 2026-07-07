#' Validate Rate-Set Metadata
#'
#' Checks rate-set metadata in a factor table. In automatic selection mode, the
#' function also checks for overlapping effective date windows within state,
#' charter, and book segment.
#'
#' @param factor_table Long-form factor table.
#' @param use_rate_set_key If `TRUE`, validate explicit key selection metadata.
#'
#' @return Invisibly returns `TRUE` if valid; otherwise errors.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' validate_rate_sets(ex$plan$factor_table, use_rate_set_key = FALSE)
validate_rate_sets <- function(factor_table, use_rate_set_key = FALSE) {
  factor_table <- as.data.frame(factor_table, stringsAsFactors = FALSE)
  required <- c("rate_set_key", "state", "charter", "book_segment", "rate_eff_date", "rate_exp_date")
  .stop_missing_cols(factor_table, required, "factor_table")

  if (!inherits(factor_table$rate_eff_date, "Date") || !inherits(factor_table$rate_exp_date, "Date")) {
    stop("rate_eff_date and rate_exp_date must be Date columns.", call. = FALSE)
  }

  if (any(factor_table$rate_eff_date > factor_table$rate_exp_date)) {
    stop("factor_table contains rows where rate_eff_date is after rate_exp_date.", call. = FALSE)
  }

  rate_cols <- required
  rate_sets <- unique(factor_table[rate_cols])

  key_meta <- unique(rate_sets[c("rate_set_key", "state", "charter", "book_segment", "rate_eff_date", "rate_exp_date")])
  key_counts <- table(key_meta$rate_set_key)
  if (any(key_counts > 1L)) {
    bad <- names(key_counts)[key_counts > 1L]
    stop("rate_set_key maps to multiple metadata rows: ", paste(bad, collapse = ", "), call. = FALSE)
  }

  if (!isTRUE(use_rate_set_key)) {
    group_cols <- c("state", "charter", "book_segment")
    groups <- unique(rate_sets[group_cols])
    for (g in seq_len(nrow(groups))) {
      idx <- rep(TRUE, nrow(rate_sets))
      for (col in group_cols) {
        idx <- idx & rate_sets[[col]] == groups[[col]][[g]]
      }
      rs <- rate_sets[idx, , drop = FALSE]
      if (nrow(rs) > 1L) {
        for (i in seq_len(nrow(rs) - 1L)) {
          for (j in (i + 1L):nrow(rs)) {
            overlaps <- rs$rate_eff_date[[i]] <= rs$rate_exp_date[[j]] && rs$rate_eff_date[[j]] <= rs$rate_exp_date[[i]]
            if (overlaps) {
              stop(
                "Overlapping automatic rate-set windows found for state/charter/book_segment: ",
                paste(groups[g, ], collapse = "/"),
                call. = FALSE
              )
            }
          }
        }
      }
    }
  }

  invisible(TRUE)
}
