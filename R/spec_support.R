.supported_value_sources <- function() c("factor_lookup", "interpolated_lookup", "input_value", "custom_function")
.supported_calculation_types <- function() c("multiplicative", "additive", "continuous_additive", "continuous_multiplicative", "replace", "custom")

.normalize_rating_spec <- function(rating_spec) {
  spec <- as.data.frame(rating_spec, stringsAsFactors = FALSE)
  .stop_missing_cols(spec, c("term_name", "calculation_type"), "rating_spec")
  if (!("step_number" %in% names(spec))) spec$step_number <- seq_len(nrow(spec))
  spec <- .add_default_col(spec, "value_source", "factor_lookup")
  spec <- .add_default_col(spec, "input_var", NA_character_)
  spec <- .add_default_col(spec, "input_vars", NA_character_)
  spec <- .add_default_col(spec, "lookup_var", NA_character_)
  spec <- .add_default_col(spec, "bounds", "error")
  spec <- .add_default_col(spec, "custom_function", NA_character_)
  spec <- .add_default_col(spec, "rounding_rule", NA_character_)
  spec <- .add_default_col(spec, "rounding_digits", NA_real_)
  spec <- .add_default_col(spec, "rounding_increment", NA_real_)
  spec <- .add_default_col(spec, "description", NA_character_)
  spec$step_number <- as.numeric(spec$step_number)
  spec$value_source <- as.character(spec$value_source)
  spec$calculation_type <- as.character(spec$calculation_type)
  spec
}

.get_spec_for_coverage <- function(spec, coverage) {
  if ("coverage" %in% names(spec)) {
    out <- spec[as.character(spec$coverage) == as.character(coverage), , drop = FALSE]
  } else {
    out <- spec
  }
  if (nrow(out) == 0) stop("No rating_spec rows found for coverage '", coverage, "'.", call. = FALSE)
  out[order(out$step_number, seq_len(nrow(out))), , drop = FALSE]
}
