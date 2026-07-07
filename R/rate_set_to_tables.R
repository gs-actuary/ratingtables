#' Convert a Rate Set to Review Tables
#'
#' Converts long-form factor rows into a named list of term tables for human
#' review or external export. This function does not write Excel files.
#'
#' @param factor_table Long-form factor table.
#' @param rate_set_key Optional rate-set key to filter.
#' @param term_names Optional character vector of terms to include.
#' @param max_vars Maximum number of variable/level slot pairs.
#'
#' @return A named list of data frames, one per term, plus `metadata`.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' rate_set_to_tables(ex$plan$factor_table, rate_set_key = "IL_HM_2025")
rate_set_to_tables <- function(factor_table, rate_set_key = NULL, term_names = NULL, max_vars = 12L) {
  ft <- as.data.frame(factor_table, stringsAsFactors = FALSE)
  if (!is.null(rate_set_key)) {
    .stop_missing_cols(ft, "rate_set_key", "factor_table")
    ft <- ft[ft$rate_set_key %in% rate_set_key, , drop = FALSE]
  }
  if (!is.null(term_names)) {
    ft <- ft[ft$term_name %in% term_names, , drop = FALSE]
  }

  meta_cols <- intersect(c("rate_set_key", "state", "charter", "book_segment", "rate_eff_date", "rate_exp_date"), names(ft))
  metadata <- unique(ft[meta_cols])

  out <- list(metadata = metadata)
  terms <- unique(as.character(ft$term_name))

  slot_cols <- .required_slot_cols(max_vars)
  for (term in terms) {
    rows <- ft[ft$term_name == term, , drop = FALSE]
    used_slots <- character()
    for (i in seq_len(max_vars)) {
      vcol <- paste0("variable", i)
      lcol <- paste0("level", i)
      if (any(!is.na(rows[[vcol]]) & rows[[vcol]] != "")) {
        used_slots <- c(used_slots, vcol, lcol)
      }
    }
    keep <- c(meta_cols, "coverage", "term_name", used_slots, "term_value")
    keep <- unique(keep[keep %in% names(rows)])
    out[[term]] <- rows[keep]
    rownames(out[[term]]) <- NULL
  }
  out
}
