#' Apply Renewal Caps to Rated Policies
#'
#' Applies upper and lower caps to indicated coverage premiums using prior
#' premium columns. Capping is intentionally separate from core rating.
#'
#' @param rated_data Data frame with `indicated_<coverage>` columns.
#' @param coverages Coverages to cap. If `NULL`, inferred from indicated columns.
#' @param prior_premium_prefix Prefix for prior premium columns.
#' @param cap_up_col Column containing maximum allowed increase as a decimal.
#' @param cap_down_col Column containing maximum allowed decrease as a decimal.
#' @param final_round_rule Optional rounding rule for final capped premium.
#'
#' @return Data frame with `final_<coverage>` columns appended.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' rated <- rate_policies(ex$policies, ex$plan)
#' apply_caps(rated, coverages = c("BI", "PD"))
apply_caps <- function(rated_data,
                       coverages = NULL,
                       prior_premium_prefix = "prior_premium_",
                       cap_up_col = "cap_up",
                       cap_down_col = "cap_down",
                       final_round_rule = "nearest_dollar") {
  out <- as.data.frame(rated_data, stringsAsFactors = FALSE)

  if (is.null(coverages)) {
    indicated_cols <- grep("^indicated_", names(out), value = TRUE)
    coverages <- sub("^indicated_", "", indicated_cols)
  }

  .stop_missing_cols(out, c(cap_up_col, cap_down_col), "rated_data")

  for (cov in coverages) {
    indicated_col <- paste0("indicated_", cov)
    prior_col <- paste0(prior_premium_prefix, cov)
    final_col <- paste0("final_", cov)
    .stop_missing_cols(out, c(indicated_col, prior_col), "rated_data")

    final_values <- numeric(nrow(out))
    for (i in seq_len(nrow(out))) {
      indicated <- out[[indicated_col]][[i]]
      prior <- out[[prior_col]][[i]]
      cap_up <- out[[cap_up_col]][[i]]
      cap_down <- out[[cap_down_col]][[i]]

      final <- indicated
      if (!is.na(prior)) {
        if (!is.na(cap_up)) {
          final <- min(final, prior * (1 + cap_up))
        }
        if (!is.na(cap_down)) {
          final <- max(final, prior * (1 - cap_down))
        }
      }
      final_values[[i]] <- round_rating_value(final, rule = final_round_rule)
    }
    out[[final_col]] <- final_values
  }

  out
}
