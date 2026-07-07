#' Find Duplicate Factor Rows
#'
#' Finds duplicate factor definitions under either explicit `rate_set_key`
#' selection or automatic rate-set selection.
#'
#' @param factor_table Long-form factor table.
#' @param max_vars Maximum number of variable/level slot pairs.
#' @param key_mode One of `"rate_set_key"` or `"automatic"`.
#'
#' @return A data frame containing duplicate rows. Returns zero rows if no
#'   duplicates are found.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' find_duplicate_factors(ex$plan$factor_table, key_mode = "automatic")
find_duplicate_factors <- function(factor_table, max_vars = 12L, key_mode = c("rate_set_key", "automatic")) {
  key_mode <- match.arg(key_mode)
  factor_table <- as.data.frame(factor_table, stringsAsFactors = FALSE)

  slot_cols <- .required_slot_cols(max_vars)
  if (key_mode == "rate_set_key") {
    key_cols <- c("rate_set_key", "coverage", "term_name", slot_cols)
  } else {
    key_cols <- c("state", "charter", "book_segment", "rate_eff_date", "rate_exp_date", "coverage", "term_name", slot_cols)
  }
  .stop_missing_cols(factor_table, key_cols, "factor_table")

  key <- factor_table[key_cols]
  dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
  factor_table[dup, , drop = FALSE]
}
