.filter_candidate_rows <- function(row, coverage, plan, term_name) {
  ft <- ensure_slot_columns(plan$factor_table, plan$max_vars)
  cand <- ft[as.character(ft$term_name) == as.character(term_name), , drop = FALSE]
  if ("coverage" %in% names(cand)) cand <- cand[as.character(cand$coverage) == as.character(coverage), , drop = FALSE]
  if (nrow(cand) == 0) return(cand)
  if (isTRUE(plan$use_rate_set_key) && "rate_set_key" %in% names(cand)) {
    key <- .get_scalar(row, "rate_set_key", NA)
    cand <- cand[as.character(cand$rate_set_key) == as.character(key), , drop = FALSE]
  } else {
    for (nm in intersect(c("state", "charter", "book_segment"), names(cand))) {
      if (nm %in% names(row)) cand <- cand[as.character(cand[[nm]]) == as.character(row[[nm]][[1]]), , drop = FALSE]
    }
    if (all(c("rate_eff_date", "rate_exp_date") %in% names(cand)) && "rating_date" %in% names(row)) {
      rd <- as.Date(row$rating_date[[1]])
      cand <- cand[as.Date(cand$rate_eff_date) <= rd & rd <= as.Date(cand$rate_exp_date), , drop = FALSE]
    }
  }
  cand
}

.row_specificity <- function(factor_row, max_vars = 12) {
  slots <- .slot_names(max_vars)
  sum(!is.na(unlist(factor_row[slots$variables], use.names = FALSE)) & nzchar(as.character(unlist(factor_row[slots$variables], use.names = FALSE))))
}

.slot_row_matches <- function(factor_row, row, max_vars = 12, ignore_var = NULL) {
  max_vars <- .normalize_max_vars(max_vars)
  for (i in seq_len(max_vars)) {
    vn <- paste0("variable", i); ln <- paste0("level", i)
    var <- factor_row[[vn]][[1]]; lvl <- factor_row[[ln]][[1]]
    if (.is_blank(var)) next
    if (!is.null(ignore_var) && as.character(var) == as.character(ignore_var)) next
    if (!(var %in% names(row))) return(FALSE)
    if (as.character(row[[var]][[1]]) != as.character(lvl)) return(FALSE)
  }
  TRUE
}

#' Look up an exact rating-table value
#'
#' Select the single most specific factor-table row that matches a rating
#' record, coverage, term, rate-set metadata, and variable-level conditions.
#'
#' @param row A one-row data frame containing the rating record.
#' @param coverage A character string identifying the coverage being rated.
#' @param plan A `rating_plan` object created by [new_rating_plan()].
#' @param term_name A character string identifying the rating term to look up.
#'
#' @return A list containing the selected numeric value, the value source,
#'   the looked-up value, and the matching factor-row identifier.
#'
#' @export
lookup_exact_value <- function(row, coverage, plan, term_name) {
  if (!inherits(plan, "rating_plan")) stop("plan must be a rating_plan object.", call. = FALSE)
  cand <- .filter_candidate_rows(row, coverage, plan, term_name)
  if (nrow(cand) == 0) stop("No factor rows found for term '", term_name, "' and coverage '", coverage, "'.", call. = FALSE)
  ok <- vapply(seq_len(nrow(cand)), function(i) .slot_row_matches(cand[i, , drop = FALSE], row, plan$max_vars), logical(1))
  matches <- cand[ok, , drop = FALSE]
  if (nrow(matches) == 0) stop("No matching factor row for term '", term_name, "'.", call. = FALSE)
  spec <- vapply(seq_len(nrow(matches)), function(i) .row_specificity(matches[i, , drop = FALSE], plan$max_vars), integer(1))
  matches <- matches[spec == max(spec), , drop = FALSE]
  if (nrow(matches) != 1) stop("Ambiguous factor lookup for term '", term_name, "'.", call. = FALSE)
  list(value = .safe_numeric(matches$term_value[[1]], "term_value"), value_source = "factor_lookup", looked_up_value = .safe_numeric(matches$term_value[[1]], "term_value"), factor_row_id = matches$factor_row_id[[1]])
}

.get_lookup_level <- function(factor_row, lookup_var, max_vars = 12) {
  for (i in seq_len(.normalize_max_vars(max_vars))) {
    if (as.character(factor_row[[paste0("variable", i)]][[1]]) == as.character(lookup_var)) return(factor_row[[paste0("level", i)]][[1]])
  }
  NA
}

#' Look up an interpolated rating-table value
#'
#' Select the applicable interpolation curve for a rating record and calculate
#' a linearly interpolated value from the surrounding table points.
#'
#' @param row A one-row data frame containing the rating record.
#' @param coverage A character string identifying the coverage being rated.
#' @param plan A `rating_plan` object created by [new_rating_plan()].
#' @param term_name A character string identifying the rating term to look up.
#' @param lookup_var A character string naming the numeric input variable used
#'   as the interpolation axis.
#' @param bounds A character string controlling values outside the available
#'   interpolation range. Supported values are `"error"`, `"clamp"`, and
#'   `"extrapolate"`.
#'
#' @return A list containing the interpolated value and supporting trace
#'   information, including the lower and upper levels, values, interpolation
#'   weight, and factor-row identifiers.
#'
#' @export
lookup_interpolated_value <- function(row, coverage, plan, term_name, lookup_var, bounds = "error") {
  if (!inherits(plan, "rating_plan")) stop("plan must be a rating_plan object.", call. = FALSE)
  if (.is_blank(lookup_var)) stop("lookup_var is required for interpolated lookup.", call. = FALSE)
  if (!(lookup_var %in% names(row))) stop("rating row is missing lookup_var '", lookup_var, "'.", call. = FALSE)
  x <- .safe_numeric(row[[lookup_var]][[1]], lookup_var)
  cand <- .filter_candidate_rows(row, coverage, plan, term_name)
  if (nrow(cand) == 0) stop("No interpolation rows found for term '", term_name, "'.", call. = FALSE)
  ok <- vapply(seq_len(nrow(cand)), function(i) .slot_row_matches(cand[i, , drop = FALSE], row, plan$max_vars, ignore_var = lookup_var), logical(1))
  cand <- cand[ok, , drop = FALSE]
  if (nrow(cand) == 0) stop("No matching interpolation curve for term '", term_name, "'.", call. = FALSE)
  xs <- vapply(seq_len(nrow(cand)), function(i) suppressWarnings(as.numeric(.get_lookup_level(cand[i, , drop = FALSE], lookup_var, plan$max_vars))), numeric(1))
  if (any(is.na(xs))) stop("Interpolation levels for term '", term_name, "' must be numeric.", call. = FALSE)
  ys <- suppressWarnings(as.numeric(cand$term_value))
  ord <- order(xs); xs <- xs[ord]; ys <- ys[ord]; cand <- cand[ord, , drop = FALSE]
  if (any(duplicated(xs))) stop("Duplicate interpolation x-values for term '", term_name, "'.", call. = FALSE)
  if (length(xs) == 1) stop("Interpolation requires at least two x-values.", call. = FALSE)
  bounds <- as.character(bounds)
  if (x < min(xs) || x > max(xs)) {
    if (bounds == "error") stop("Interpolation input for term '", term_name, "' is outside table bounds.", call. = FALSE)
    if (bounds == "clamp") x <- min(max(x, min(xs)), max(xs))
    if (!(bounds %in% c("error", "clamp", "extrapolate"))) stop("Unsupported interpolation bounds: ", bounds, call. = FALSE)
  }
  if (x %in% xs) {
    idx <- which(xs == x)[1]
    return(list(value = ys[idx], value_source = "interpolated_lookup", looked_up_value = ys[idx], input_var = lookup_var, input_value = x, lower_level = xs[idx], upper_level = xs[idx], lower_value = ys[idx], upper_value = ys[idx], interpolation_weight = 0, lower_factor_row_id = cand$factor_row_id[[idx]], upper_factor_row_id = cand$factor_row_id[[idx]]))
  }
  upper_idx <- which(xs > x)[1]
  lower_idx <- upper_idx - 1
  if (is.na(upper_idx)) { upper_idx <- length(xs); lower_idx <- upper_idx - 1 }
  if (lower_idx < 1) { lower_idx <- 1; upper_idx <- 2 }
  w <- (x - xs[lower_idx]) / (xs[upper_idx] - xs[lower_idx])
  val <- ys[lower_idx] + w * (ys[upper_idx] - ys[lower_idx])
  list(value = val, value_source = "interpolated_lookup", looked_up_value = val, input_var = lookup_var, input_value = x, lower_level = xs[lower_idx], upper_level = xs[upper_idx], lower_value = ys[lower_idx], upper_value = ys[upper_idx], interpolation_weight = w, lower_factor_row_id = cand$factor_row_id[[lower_idx]], upper_factor_row_id = cand$factor_row_id[[upper_idx]])
}

#' Look up a factor value by source
#'
#' Dispatch a rating-table lookup to either exact matching or interpolated
#' lookup according to `value_source`.
#'
#' @param row A one-row data frame containing the rating record.
#' @param coverage A character string identifying the coverage being rated.
#' @param plan A `rating_plan` object created by [new_rating_plan()].
#' @param term_name A character string identifying the rating term to look up.
#' @param value_source A character string specifying the lookup method.
#'   Supported values are `"factor_lookup"` and `"interpolated_lookup"`.
#' @param lookup_var An optional character string naming the interpolation
#'   variable. Required for `value_source = "interpolated_lookup"`.
#' @param bounds A character string controlling out-of-range interpolation.
#'   Supported values are `"error"`, `"clamp"`, and `"extrapolate"`.
#'
#' @return A list containing the selected or interpolated rating value and
#'   associated trace information.
#'
#' @export
lookup_factor_value <- function(row, coverage, plan, term_name, value_source = "factor_lookup", lookup_var = NULL, bounds = "error") {
  if (value_source == "factor_lookup") return(lookup_exact_value(row, coverage, plan, term_name))
  if (value_source == "interpolated_lookup") return(lookup_interpolated_value(row, coverage, plan, term_name, lookup_var, bounds))
  stop("lookup_factor_value supports factor_lookup and interpolated_lookup only.", call. = FALSE)
}

#' Look up an exact rating term value
#'
#' Compatibility wrapper around [lookup_exact_value()]. By default, it returns
#' only the numeric factor value.
#'
#' @param row A one-row data frame containing the rating record.
#' @param coverage A character string identifying the coverage being rated.
#' @param plan A `rating_plan` object created by [new_rating_plan()].
#' @param term_name A character string identifying the rating term to look up.
#' @param return_match Logical. If `TRUE`, return the complete lookup result;
#'   otherwise return only its numeric value.
#' @param ... Additional arguments accepted for backward compatibility.
#'   They are currently ignored.
#'
#' @return If `return_match = FALSE`, a numeric rating value. If
#'   `return_match = TRUE`, a list containing the value and matching-row
#'   information.
#'
#' @export
lookup_term_value <- function(row, coverage, plan, term_name, return_match = FALSE, ...) {
  ans <- lookup_exact_value(row, coverage, plan, term_name)
  if (isTRUE(return_match)) return(ans)
  ans$value
}

.make_lookup_helper <- function(row, coverage, plan) {
  force(row); force(coverage); force(plan)
  function(term_name, value_source = "factor_lookup", lookup_var = NULL, bounds = "error") {
    lookup_factor_value(row, coverage, plan, term_name, value_source, lookup_var, bounds)
  }
}
