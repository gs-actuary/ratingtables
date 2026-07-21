#' Validate a factor table
#' @param factor_table A factor table.
#' @param max_vars Maximum slot count.
#' @export
validate_factor_table <- function(factor_table, max_vars = 12) {
  ft <- ensure_slot_columns(as.data.frame(factor_table, stringsAsFactors = FALSE), max_vars)
  .stop_missing_cols(ft, c("term_name", "term_value"), "factor_table")
  bad <- suppressWarnings(is.na(as.numeric(ft$term_value)))
  if (any(bad)) stop("factor_table$term_value must be numeric or coercible to numeric.", call. = FALSE)
  invisible(TRUE)
}

#' Find duplicate factor-table rows
#'
#' Identify factor-table rows that have duplicate lookup keys.
#'
#' @param factor_table A normalized long-form factor table.
#' @param max_vars Maximum number of variable-level slot pairs to inspect.
#' @param ... Additional arguments accepted for backward compatibility.
#'
#' @return A data frame containing factor-table rows with duplicated lookup
#'   keys. An empty data frame is returned when no duplicates are found.
#'
#' @export
find_duplicate_factors <- function(factor_table, max_vars = 12, ...) {
  ft <- ensure_slot_columns(as.data.frame(factor_table, stringsAsFactors = FALSE), max_vars)
  slots <- .slot_names(max_vars)
  key_cols <- intersect(c("rate_set_key", "state", "charter", "book_segment", "rate_eff_date", "rate_exp_date", "coverage", "term_name", as.vector(rbind(slots$variables, slots$levels))), names(ft))
  key <- do.call(paste, c(ft[key_cols], sep = "\r"))
  ft[duplicated(key) | duplicated(key, fromLast = TRUE), , drop = FALSE]
}

#' Validate a rating specification
#' @param rating_spec Rating spec.
#' @param ... Ignored for compatibility.
#' @export
validate_rating_spec <- function(rating_spec, ...) {
  spec <- .normalize_rating_spec(rating_spec)
  bad_vs <- setdiff(unique(as.character(spec$value_source)), .supported_value_sources())
  if (length(bad_vs) > 0) stop("Unsupported value_source(s): ", paste(bad_vs, collapse = ", "), call. = FALSE)
  bad_calc <- setdiff(unique(as.character(spec$calculation_type)), .supported_calculation_types())
  if (length(bad_calc) > 0) stop("Unsupported calculation_type(s): ", paste(bad_calc, collapse = ", "), call. = FALSE)
  interp <- spec$value_source == "interpolated_lookup"
  if (any(interp)) {
    lv <- ifelse(!is.na(spec$lookup_var[interp]) & nzchar(spec$lookup_var[interp]), spec$lookup_var[interp], spec$input_var[interp])
    if (any(is.na(lv) | !nzchar(lv))) stop("interpolated_lookup rows require lookup_var or input_var.", call. = FALSE)
  }
  input <- spec$value_source == "input_value"
  if (any(input) && any(is.na(spec$input_var[input]) | !nzchar(spec$input_var[input]))) stop("input_value rows require input_var.", call. = FALSE)
  custom <- spec$value_source == "custom_function"
  if (any(custom) && any(is.na(spec$custom_function[custom]) | !nzchar(spec$custom_function[custom]))) stop("custom_function rows require custom_function.", call. = FALSE)
  invisible(TRUE)
}

#' Validate rate set fields
#' @param factor_table A factor table.
#' @export
validate_rate_sets <- function(factor_table) {
  ft <- as.data.frame(factor_table, stringsAsFactors = FALSE)
  if ("rate_set_key" %in% names(ft) && any(is.na(ft$rate_set_key) | !nzchar(as.character(ft$rate_set_key)))) stop("rate_set_key cannot be blank when present.", call. = FALSE)
  if (all(c("rate_eff_date", "rate_exp_date") %in% names(ft)) && any(as.Date(ft$rate_eff_date) > as.Date(ft$rate_exp_date))) stop("rate_eff_date must be on or before rate_exp_date.", call. = FALSE)
  invisible(TRUE)
}

#' Validate a rating plan
#' @param plan A rating plan.
#' @export
validate_rating_plan <- function(plan) {
  if (!inherits(plan, "rating_plan")) stop("plan must be a rating_plan.", call. = FALSE)
  validate_factor_table(plan$factor_table, plan$max_vars)
  validate_rating_spec(plan$rating_spec)
  validate_rate_sets(plan$factor_table)
  custom_rows <- plan$rating_spec$value_source == "custom_function"
  if (any(custom_rows)) {
    fn_names <- unique(as.character(plan$rating_spec$custom_function[custom_rows]))
    missing <- setdiff(fn_names, names(plan$custom_functions))
    if (length(missing) > 0) stop("custom_function(s) not found in plan$custom_functions: ", paste(missing, collapse = ", "), call. = FALSE)
    not_fun <- fn_names[!vapply(plan$custom_functions[fn_names], is.function, logical(1))]
    if (length(not_fun) > 0) stop("custom_functions entries must be functions: ", paste(not_fun, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' Required policy fields for a plan
#' @param plan A rating plan.
#' @export
required_policy_fields <- function(plan) {
  if (!inherits(plan, "rating_plan")) stop("plan must be a rating_plan.", call. = FALSE)
  fields <- character(0)
  ft <- ensure_slot_columns(plan$factor_table, plan$max_vars)
  slots <- .slot_names(plan$max_vars)
  vars <- unique(unlist(ft[slots$variables], use.names = FALSE))
  vars <- vars[!is.na(vars) & nzchar(as.character(vars))]
  fields <- c(fields, as.character(vars))
  spec <- plan$rating_spec
  input_vars <- unique(c(as.character(spec$input_var), unlist(lapply(spec$input_vars, .split_csv), use.names = FALSE), as.character(spec$lookup_var)))
  input_vars <- input_vars[!is.na(input_vars) & nzchar(input_vars)]
  fields <- c(fields, input_vars)
  if (isTRUE(plan$use_rate_set_key)) fields <- c(fields, "rate_set_key") else {
    auto <- c("state", "charter", "book_segment", "rating_date")
    auto <- auto[auto %in% names(ft)]
    fields <- c(fields, auto)
  }
  unique(fields)
}

#' Validate rating input data
#' @param rating_data Input data.
#' @param plan A rating plan.
#' @export
validate_policy_data <- function(rating_data, plan) {
  d <- as.data.frame(rating_data, stringsAsFactors = FALSE)
  .stop_missing_cols(d, required_policy_fields(plan), "rating_data")
  invisible(TRUE)
}
