#' Average Entity Factors by Group
#'
#' Averages `indicated_<coverage>` columns within a grouping variable. This is
#' useful for driver averaging or similar entity-to-policy aggregation.
#'
#' @param scored_entity_data Data frame with `indicated_<coverage>` columns.
#' @param group_col Grouping column name.
#' @param coverages Coverage names. If `NULL`, inferred from indicated columns.
#' @param output_prefix Prefix for output factor columns.
#'
#' @return A data frame with one row per group and averaged factor columns.
#' @export
#'
#' @examples
#' d <- data.frame(household_id = c("H1", "H1", "H2"), indicated_BI = c(1.1, 0.9, 1.2))
#' average_entity_factors(d, group_col = "household_id", coverages = "BI")
average_entity_factors <- function(scored_entity_data,
                                   group_col,
                                   coverages = NULL,
                                   output_prefix = "avg_factor_") {
  d <- as.data.frame(scored_entity_data, stringsAsFactors = FALSE)
  .stop_missing_cols(d, group_col, "scored_entity_data")

  if (is.null(coverages)) {
    indicated_cols <- grep("^indicated_", names(d), value = TRUE)
    coverages <- sub("^indicated_", "", indicated_cols)
  }

  groups <- unique(d[[group_col]])
  out <- data.frame(groups, stringsAsFactors = FALSE)
  names(out)[1L] <- group_col

  for (cov in coverages) {
    col <- paste0("indicated_", cov)
    .stop_missing_cols(d, col, "scored_entity_data")
    vals <- tapply(d[[col]], d[[group_col]], mean, na.rm = TRUE)
    out[[paste0(output_prefix, cov)]] <- as.numeric(vals[as.character(out[[group_col]])])
  }

  rownames(out) <- NULL
  out
}
