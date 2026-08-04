#' Apply a rounding rule
#' @param x A numeric vector.
#' @param rule A character string naming the rounding rule. Supported values
#'   are `"none"`, `"round"`, `"floor"`, `"ceiling"`, `"nearest_dollar"`,
#'   `"nearest_cent"`, `"nearest_dime"`, and `"nearest_increment"`. A missing
#'   value also leaves `x` unchanged.
#' @param digits The number of decimal places used when `rule = "round"`.
#'   A missing value defaults to zero.
#' @param increment The numeric increment used when
#'   `rule = "nearest_increment"`.
#' 
#' @return A numeric vector containing the rounded values. If `rule` is
#'   missing or `"none"`, `x` is returned unchanged.
#'   
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
