#' Validate a Long-Form Factor Table
#'
#' Checks basic schema and type requirements for a factor table.
#'
#' @param factor_table Long-form factor table.
#' @param max_vars Maximum number of variable/level slot pairs.
#'
#' @return Invisibly returns `TRUE` if valid; otherwise errors.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' validate_factor_table(ex$plan$factor_table)
validate_factor_table <- function(factor_table, max_vars = 12L) {
  factor_table <- as.data.frame(factor_table, stringsAsFactors = FALSE)
  required <- c("coverage", "term_name", "term_value", .required_slot_cols(max_vars))
  .stop_missing_cols(factor_table, required, "factor_table")

  if (!is.numeric(factor_table$term_value)) {
    stop("factor_table$term_value must be numeric.", call. = FALSE)
  }

  if (any(is.na(factor_table$coverage) | factor_table$coverage == "")) {
    stop("factor_table contains missing coverage values.", call. = FALSE)
  }

  if (any(is.na(factor_table$term_name) | factor_table$term_name == "")) {
    stop("factor_table contains missing term_name values.", call. = FALSE)
  }

  if (any(is.na(factor_table$term_value))) {
    stop("factor_table contains missing term_value values.", call. = FALSE)
  }

  for (i in seq_len(max_vars)) {
    v <- factor_table[[paste0("variable", i)]]
    l <- factor_table[[paste0("level", i)]]
    has_var <- !(is.na(v) | v == "")
    has_level <- !(is.na(l) | l == "")
    if (any(xor(has_var, has_level))) {
      stop("Each populated variable slot must have a populated level slot, and vice versa. Problem at slot ", i, ".", call. = FALSE)
    }
  }

  invisible(TRUE)
}
