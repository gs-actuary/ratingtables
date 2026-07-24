#' Apply a rounding rule
#' @param x Numeric vector.
#' @param rule Rounding rule.
#' @param digits Digits for base R rounding rules.
#' @param increment Increment for nearest_increment.
#' @examples
#' premiums <- c(101.234, 105.678)
#'
#' apply_rounding(
#'   premiums,
#'   rule = "nearest_cent"
#' )
#'
#' apply_rounding(
#'   premiums,
#'   rule = "nearest_increment",
#'   increment = 5
#' )
#' @export
apply_rounding <- function(x, rule = NA, digits = NA, increment = NA) {
  if (.is_blank(rule) || identical(rule, "none")) return(x)
  rule <- as.character(rule)
  if (rule == "round") return(round(x, ifelse(is.na(digits), 0, digits)))
  if (rule == "floor") return(floor(x))
  if (rule == "ceiling") return(ceiling(x))
  if (rule == "nearest_dollar") return(round(x, 0))
  if (rule == "nearest_cent") return(round(x, 2))
  if (rule == "nearest_dime") return(round(x / 0.10) * 0.10)
  if (rule == "nearest_increment") {
    inc <- .safe_numeric(increment, "rounding_increment")
    return(round(x / inc) * inc)
  }
  stop("Unsupported rounding_rule: ", rule, call. = FALSE)
}

.apply_step_rounding <- function(x, spec_row) {
  apply_rounding(x, .get_scalar(spec_row, "rounding_rule", NA), .get_scalar(spec_row, "rounding_digits", NA), .get_scalar(spec_row, "rounding_increment", NA))
}
