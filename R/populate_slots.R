#' Populate Variable/Level Slots
#'
#' Populates the long-form factor-table matching slots. The names of
#' `variable_map` are the variable names expected in policy data, and the
#' values are the columns in `df` containing the corresponding factor levels.
#'
#' @param df A data frame containing source level columns.
#' @param variable_map A named character vector. Names are policy variable
#'   names; values are columns in `df` containing the levels.
#' @param max_vars Maximum number of variable/level slot pairs.
#'
#' @return A data frame with populated `variable*` and `level*` columns.
#' @export
#'
#' @examples
#' x <- data.frame(territory = c("T1", "T2"), term_value = c(100, 125))
#' populate_slots(x, c(territory = "territory"), max_vars = 2)
populate_slots <- function(df, variable_map, max_vars = 12L) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df <- make_empty_slots(df, max_vars = max_vars)

  if (length(variable_map) > max_vars) {
    stop("variable_map has more entries than max_vars.", call. = FALSE)
  }

  if (length(variable_map) > 0L) {
    if (is.null(names(variable_map)) || any(names(variable_map) == "")) {
      stop("variable_map must be a named character vector.", call. = FALSE)
    }
    missing_cols <- setdiff(as.character(variable_map), names(df))
    if (length(missing_cols) > 0L) {
      stop("df is missing mapped column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    for (i in seq_along(variable_map)) {
      df[[paste0("variable", i)]] <- names(variable_map)[[i]]
      df[[paste0("level", i)]] <- as.character(df[[variable_map[[i]]]])
    }
  }

  df
}
