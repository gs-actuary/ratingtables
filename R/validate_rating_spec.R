#' Validate a Rating Specification
#'
#' Checks that a rating specification has valid terms, calculation types, and
#' rounding rules.
#'
#' @param rating_spec Rating specification data frame.
#' @param factor_table Optional factor table used to verify that spec terms are
#'   present in the factor table.
#'
#' @return Invisibly returns `TRUE` if valid; otherwise errors.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' validate_rating_spec(ex$plan$rating_spec, ex$plan$factor_table)
validate_rating_spec <- function(rating_spec, factor_table = NULL) {
  rating_spec <- as.data.frame(rating_spec, stringsAsFactors = FALSE)
  required <- c("term_name", "calculation_type", "continuous_var")
  .stop_missing_cols(rating_spec, required, "rating_spec")

  if (nrow(rating_spec) == 0L) {
    stop("rating_spec must have at least one row.", call. = FALSE)
  }

  if (any(is.na(rating_spec$term_name) | rating_spec$term_name == "")) {
    stop("rating_spec contains missing term_name values.", call. = FALSE)
  }

  allowed <- .allowed_calculation_types()
  bad <- setdiff(unique(as.character(rating_spec$calculation_type)), allowed)
  if (length(bad) > 0L) {
    if ("continuous" %in% bad) {
      stop("Unsupported calculation_type 'continuous'. Use 'continuous_additive' or 'continuous_multiplicative'.", call. = FALSE)
    }
    stop("Unsupported calculation_type value(s): ", paste(bad, collapse = ", "), call. = FALSE)
  }

  continuous_rows <- rating_spec$calculation_type %in% c("continuous_additive", "continuous_multiplicative")
  if (any(continuous_rows)) {
    cv <- rating_spec$continuous_var[continuous_rows]
    if (any(is.na(cv) | cv == "")) {
      stop("continuous_additive and continuous_multiplicative rows must supply continuous_var.", call. = FALSE)
    }
  }

  if ("rounding_rule" %in% names(rating_spec)) {
    rr <- as.character(rating_spec$rounding_rule)
    rr <- rr[!is.na(rr) & rr != ""]
    bad_rr <- setdiff(unique(rr), .allowed_rounding_rules())
    if (length(bad_rr) > 0L) {
      stop("Unsupported rounding_rule value(s): ", paste(bad_rr, collapse = ", "), call. = FALSE)
    }
  }

  if (!is.null(factor_table)) {
    terms_missing <- setdiff(unique(as.character(rating_spec$term_name)), unique(as.character(factor_table$term_name)))
    if (length(terms_missing) > 0L) {
      stop("rating_spec term(s) missing from factor_table: ", paste(terms_missing, collapse = ", "), call. = FALSE)
    }
  }

  invisible(TRUE)
}
