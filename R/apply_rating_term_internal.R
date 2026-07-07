# Internal term application helper ----------------------------------------

.apply_rating_term <- function(current_value,
                               looked_up_value,
                               calculation_type,
                               continuous_input = NA_real_) {
  if (calculation_type == "multiplicative") {
    contribution <- looked_up_value
    if (is.na(current_value)) {
      current_value <- looked_up_value
    } else {
      current_value <- current_value * looked_up_value
    }
    return(list(value = current_value, contribution = contribution, multiplier = looked_up_value))
  }

  if (calculation_type == "additive") {
    contribution <- looked_up_value
    if (is.na(current_value)) {
      current_value <- looked_up_value
    } else {
      current_value <- current_value + looked_up_value
    }
    return(list(value = current_value, contribution = contribution, multiplier = NA_real_))
  }

  if (calculation_type == "continuous_additive") {
    contribution <- looked_up_value * continuous_input
    if (is.na(current_value)) {
      current_value <- contribution
    } else {
      current_value <- current_value + contribution
    }
    return(list(value = current_value, contribution = contribution, multiplier = NA_real_))
  }

  if (calculation_type == "continuous_multiplicative") {
    multiplier <- 1 + looked_up_value * continuous_input
    contribution <- multiplier
    if (is.na(current_value)) {
      current_value <- multiplier
    } else {
      current_value <- current_value * multiplier
    }
    return(list(value = current_value, contribution = contribution, multiplier = multiplier))
  }

  stop("Unsupported calculation_type: ", calculation_type, call. = FALSE)
}
