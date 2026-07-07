#' Round Rating Values
#'
#' Applies a named rounding rule to numeric values. Rounding is optional: if
#' `rule` is missing, `NA`, or empty, `x` is returned unchanged.
#'
#' @param x Numeric vector.
#' @param rule Rounding rule. Supported values are `nearest_cent`,
#'   `nearest_dollar`, `up_dollar`, `down_dollar`, `digits`,
#'   `nearest_increment`, and `nearest_dime`.
#' @param digits Number of digits for `rule = "digits"`.
#' @param increment Increment for `rule = "nearest_increment"`.
#'
#' @return Rounded numeric vector.
#' @export
#'
#' @examples
#' round_rating_value(c(12.34, 12.35), "nearest_dime")
#' round_rating_value(12.345, "nearest_cent")
#' round_rating_value(12.345, "nearest_increment", increment = 0.25)
round_rating_value <- function(x, rule = NA_character_, digits = NA_real_, increment = NA_real_) {
  if (length(rule) == 0L || is.na(rule) || is.null(rule) || rule == "") {
    return(x)
  }

  if (rule == "nearest_cent") {
    return(round(x, 2L))
  }

  if (rule == "nearest_dollar") {
    return(round(x, 0L))
  }

  if (rule == "up_dollar") {
    return(ceiling(x))
  }

  if (rule == "down_dollar") {
    return(floor(x))
  }

  if (rule == "digits") {
    if (length(digits) == 0L || is.na(digits)) {
      stop("digits must be supplied when rule = 'digits'.", call. = FALSE)
    }
    return(round(x, digits = digits))
  }

  if (rule == "nearest_increment") {
    if (length(increment) == 0L || is.na(increment) || increment <= 0) {
      stop("increment must be a positive number when rule = 'nearest_increment'.", call. = FALSE)
    }
    return(round(x / increment) * increment)
  }

  if (rule == "nearest_dime") {
    return(round(x / 0.10) * 0.10)
  }

  stop("Unsupported rounding rule: ", rule, call. = FALSE)
}
