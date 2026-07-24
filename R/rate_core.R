.evaluate_custom_function <- function(row, coverage, current_premium, plan, spec_row) {
  fn_name <- as.character(.get_scalar(spec_row, "custom_function", NA))
  if (.is_blank(fn_name)) stop("custom_function spec row is missing custom_function name.", call. = FALSE)
  fn <- plan$custom_functions[[fn_name]]
  if (!is.function(fn)) stop("custom_function not found or not a function: ", fn_name, call. = FALSE)
  lookup <- .make_lookup_helper(row, coverage, plan)
  ans <- fn(row = row, coverage = coverage, current_premium = current_premium, plan = plan, spec_row = spec_row, lookup = lookup)
  extra_trace <- NULL
  if (is.list(ans) && !is.null(ans$value)) {
    val <- ans$value
    if (!is.null(ans$trace)) extra_trace <- as.data.frame(ans$trace, stringsAsFactors = FALSE)
  } else val <- ans
  list(value = .safe_numeric(val, paste0("custom_function ", fn_name, " return value")), value_source = "custom_function", custom_function = fn_name, custom_trace = extra_trace)
}

.get_step_value <- function(row, coverage, plan, spec_row, current_premium) {
  vs <- as.character(.get_scalar(spec_row, "value_source", "factor_lookup"))
  term <- as.character(.get_scalar(spec_row, "term_name", NA))
  if (vs == "factor_lookup") return(lookup_exact_value(row, coverage, plan, term))
  if (vs == "interpolated_lookup") {
    lv <- .first_nonblank(.get_scalar(spec_row, "lookup_var", NA), .get_scalar(spec_row, "input_var", NA))
    return(lookup_interpolated_value(row, coverage, plan, term, lv, .get_scalar(spec_row, "bounds", "error")))
  }
  if (vs == "input_value") {
    input_var <- as.character(.get_scalar(spec_row, "input_var", NA))
    if (.is_blank(input_var)) stop("input_value row requires input_var.", call. = FALSE)
    if (!(input_var %in% names(row))) stop("rating row is missing input_var '", input_var, "'.", call. = FALSE)
    val <- .safe_numeric(row[[input_var]][[1]], input_var)
    return(list(value = val, value_source = "input_value", input_var = input_var, input_value = val, looked_up_value = NA_real_))
  }
  if (vs == "custom_function") return(.evaluate_custom_function(row, coverage, current_premium, plan, spec_row))
  stop("Unsupported value_source: ", vs, call. = FALSE)
}

.apply_step_value <- function(current_premium, step_value, row, spec_row) {
  calc <- as.character(.get_scalar(spec_row, "calculation_type", "multiplicative"))
  input_var <- as.character(.get_scalar(spec_row, "input_var", NA))
  before <- current_premium
  value <- .safe_numeric(step_value$value, "step value")
  input_value <- if (!is.null(step_value$input_value)) step_value$input_value else NA_real_
  applied_value <- value
  if (calc == "multiplicative") {
    base <- if (is.na(current_premium)) 1 else current_premium
    after <- base * value
  } else if (calc == "additive") {
    base <- if (is.na(current_premium)) 0 else current_premium
    after <- base + value
  } else if (calc == "continuous_additive") {
    if (.is_blank(input_var)) stop("continuous_additive requires input_var.", call. = FALSE)
    if (!(input_var %in% names(row))) stop("rating row is missing input_var '", input_var, "'.", call. = FALSE)
    input_value <- .safe_numeric(row[[input_var]][[1]], input_var)
    applied_value <- value * input_value
    after <- (if (is.na(current_premium)) 0 else current_premium) + applied_value
  } else if (calc == "continuous_multiplicative") {
    if (.is_blank(input_var)) stop("continuous_multiplicative requires input_var.", call. = FALSE)
    if (!(input_var %in% names(row))) stop("rating row is missing input_var '", input_var, "'.", call. = FALSE)
    input_value <- .safe_numeric(row[[input_var]][[1]], input_var)
    applied_value <- 1 + value * input_value
    after <- (if (is.na(current_premium)) 1 else current_premium) * applied_value
  } else if (calc == "replace") {
    after <- value
  } else if (calc == "custom") {
    after <- value
  } else stop("Unsupported calculation_type: ", calc, call. = FALSE)
  after <- .apply_step_rounding(after, spec_row)
  list(before = before, after = after, input_value = input_value, applied_value = applied_value)
}

.make_trace_row <- function(row_number, record_id, coverage, spec_row, step_value, applied) {
  data.frame(row_number = row_number, record_id = record_id, coverage = coverage, step_number = .get_scalar(spec_row, "step_number", NA), term_name = as.character(.get_scalar(spec_row, "term_name", NA)), value_source = as.character(.get_scalar(spec_row, "value_source", "factor_lookup")), calculation_type = as.character(.get_scalar(spec_row, "calculation_type", NA)), input_var = .first_nonblank(step_value$input_var, .get_scalar(spec_row, "input_var", NA)), input_value = if (!is.null(applied$input_value)) applied$input_value else NA_real_, looked_up_value = if (!is.null(step_value$looked_up_value)) step_value$looked_up_value else ifelse(step_value$value_source %in% c("factor_lookup", "interpolated_lookup"), step_value$value, NA_real_), applied_value = applied$applied_value, value_before_step = applied$before, value_after_step = applied$after, factor_row_id = if (!is.null(step_value$factor_row_id)) step_value$factor_row_id else NA, lower_level = if (!is.null(step_value$lower_level)) step_value$lower_level else NA, upper_level = if (!is.null(step_value$upper_level)) step_value$upper_level else NA, lower_value = if (!is.null(step_value$lower_value)) step_value$lower_value else NA, upper_value = if (!is.null(step_value$upper_value)) step_value$upper_value else NA, interpolation_weight = if (!is.null(step_value$interpolation_weight)) step_value$interpolation_weight else NA_real_, custom_function = if (!is.null(step_value$custom_function)) step_value$custom_function else NA_character_, stringsAsFactors = FALSE)
}

#' Rate one record for one coverage
#'
#' Execute the applicable rating specification one step at a time for a single
#' record and coverage.
#'
#' @param row A one-row data frame containing the rating record.
#' @param coverage A character string identifying the coverage to rate.
#' @param plan A `rating_plan` object created by [new_rating_plan()].
#' @param row_number An integer identifying the row within the source rating
#'   data. This value is included in the trace output.
#'
#' @return A list with two elements: `value`, containing the final indicated
#'   value, and `trace`, containing one trace row per rating-specification step.
#' @examples
#' ex <- example_rating_plan()
#'
#' answer <- rate_one_row_one_coverage(
#'   row = ex$policies[1, , drop = FALSE],
#'   coverage = "BI",
#'   plan = ex$plan,
#'   row_number = 1
#' )
#'
#' answer$value
#' answer$trace
#' @export
rate_one_row_one_coverage <- function(row, coverage, plan, row_number = 1) {
  if (!inherits(plan, "rating_plan")) stop("plan must be a rating_plan object.", call. = FALSE)
  row <- as.data.frame(row, stringsAsFactors = FALSE)
  spec <- .get_spec_for_coverage(plan$rating_spec, coverage)
  prem <- NA_real_
  traces <- list()
  record_id <- if (plan$policy_id_col %in% names(row)) as.character(row[[plan$policy_id_col]][[1]]) else as.character(row_number)
  for (i in seq_len(nrow(spec))) {
    sr <- spec[i, , drop = FALSE]
    sv <- .get_step_value(row, coverage, plan, sr, prem)
    ap <- .apply_step_value(prem, sv, row, sr)
    traces[[i]] <- .make_trace_row(row_number, record_id, coverage, sr, sv, ap)
    prem <- ap$after
  }
  list(value = prem, trace = .combine_rows(traces))
}
