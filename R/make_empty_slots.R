#' Add Empty Variable/Level Slots to a Factor Table
#'
#' Adds `variable1`, `level1`, ..., `variableN`, `levelN` columns to a data
#' frame. These columns are the matching slots used by the long-form factor
#' table.
#'
#' @param df A data frame.
#' @param max_vars Maximum number of variable/level slot pairs to create.
#'
#' @return A data frame with empty slot columns added or overwritten.
#' @export
#'
#' @examples
#' x <- data.frame(term_name = "territory", term_value = 100)
#' make_empty_slots(x, max_vars = 2)
make_empty_slots <- function(df, max_vars = 12L) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (i in seq_len(max_vars)) {
    df[[paste0("variable", i)]] <- NA_character_
    df[[paste0("level", i)]] <- NA_character_
  }
  df
}
