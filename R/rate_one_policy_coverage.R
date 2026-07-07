#' Rate One Policy Row and Coverage
#'
#' Rates a single policy row for a single coverage using a rating plan.
#'
#' `continuous_additive` means `coefficient * input` is added to the running
#' value. `continuous_multiplicative` means the running value is multiplied by
#' `1 + coefficient * input`.
#'
#' @param policy_row One-row data frame or named list.
#' @param plan A `rating_plan` object.
#' @param coverage Coverage to rate.
#' @param return_trace If `TRUE`, return trace tables.
#' @param trace_detail One of `"terms"`, `"matches"`, or `"all"`.
#' @param row_id Optional input row identifier for trace output.
#' @param policy_id Optional policy identifier for trace output.
#' @param stage Stage label for trace output.
#' @param stage_entity_type Entity type label for trace output.
#' @param stage_entity_id Entity ID for trace output.
#'
#' @return A numeric premium if `return_trace = FALSE`; otherwise a list with
#'   `final_value`, `term_trace`, and possibly `factor_match_trace`.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' rate_one_policy_coverage(ex$policies[1, ], ex$plan, "BI")
#' rate_one_policy_coverage(ex$policies[1, ], ex$plan, "BI", return_trace = TRUE)
rate_one_policy_coverage <- function(policy_row,
                                     plan,
                                     coverage,
                                     return_trace = FALSE,
                                     trace_detail = c("terms", "matches", "all"),
                                     row_id = NA_integer_,
                                     policy_id = NA,
                                     stage = "main_rating",
                                     stage_entity_type = "policy",
                                     stage_entity_id = NA) {
  if (!inherits(plan, "rating_plan")) {
    stop("plan must be a rating_plan object.", call. = FALSE)
  }
  trace_detail <- match.arg(trace_detail)
  policy_row <- .normalize_policy_row(policy_row)

  current_value <- NA_real_
  term_trace_list <- vector("list", nrow(plan$rating_spec))
  match_trace_list <- list()

  for (i in seq_len(nrow(plan$rating_spec))) {
    spec_row <- plan$rating_spec[i, , drop = FALSE]
    term_name <- as.character(spec_row$term_name[[1L]])
    calculation_type <- as.character(spec_row$calculation_type[[1L]])
    continuous_var <- if ("continuous_var" %in% names(spec_row)) as.character(spec_row$continuous_var[[1L]]) else NA_character_
    rounding_rule <- if ("rounding_rule" %in% names(spec_row)) as.character(spec_row$rounding_rule[[1L]]) else NA_character_
    rounding_digits <- if ("rounding_digits" %in% names(spec_row)) spec_row$rounding_digits[[1L]] else NA_real_
    rounding_increment <- if ("rounding_increment" %in% names(spec_row)) spec_row$rounding_increment[[1L]] else NA_real_

    lookup <- lookup_term_value(
      policy_row = policy_row,
      plan = plan,
      term_name = term_name,
      coverage = coverage,
      return_match = TRUE
    )

    looked_up_value <- lookup$term_value
    value_before_step <- current_value
    continuous_input <- NA_real_

    if (calculation_type %in% c("continuous_additive", "continuous_multiplicative")) {
      if (is.na(continuous_var) || continuous_var == "") {
        stop("Spec row for term '", term_name, "' is continuous but continuous_var is missing.", call. = FALSE)
      }
      if (!(continuous_var %in% names(policy_row))) {
        stop("Policy row does not contain continuous variable '", continuous_var, "' needed by term '", term_name, "'.", call. = FALSE)
      }
      continuous_input <- as.numeric(.get_policy_value(policy_row, continuous_var))
      if (is.na(continuous_input)) {
        stop("Continuous input for variable '", continuous_var, "' is NA in the policy row.", call. = FALSE)
      }
    }

    applied <- .apply_rating_term(
      current_value = current_value,
      looked_up_value = looked_up_value,
      calculation_type = calculation_type,
      continuous_input = continuous_input
    )
    current_value <- applied$value

    if (isTRUE(plan$rounding_enabled)) {
      current_value <- round_rating_value(
        x = current_value,
        rule = rounding_rule,
        digits = rounding_digits,
        increment = rounding_increment
      )
    }

    if (isTRUE(return_trace)) {
      term_trace_list[[i]] <- data.frame(
        row_id = row_id,
        policy_id = policy_id,
        coverage = coverage,
        stage = stage,
        stage_entity_type = stage_entity_type,
        stage_entity_id = stage_entity_id,
        step_number = i,
        term_name = term_name,
        calculation_type = calculation_type,
        looked_up_value = looked_up_value,
        continuous_var = continuous_var,
        continuous_input = continuous_input,
        contribution = applied$contribution,
        multiplier = applied$multiplier,
        value_before_step = value_before_step,
        value_after_step = current_value,
        rounding_rule = rounding_rule,
        rate_set_key = lookup$rate_set_key,
        factor_row_id = lookup$factor_row_id,
        stringsAsFactors = FALSE
      )

      if (trace_detail %in% c("matches", "all") && nrow(lookup$match_trace) > 0L) {
        mt <- lookup$match_trace
        mt$row_id <- row_id
        mt$policy_id <- policy_id
        mt$stage <- stage
        mt$stage_entity_type <- stage_entity_type
        mt$stage_entity_id <- stage_entity_id
        mt$step_number <- i
        mt <- mt[c("row_id", "policy_id", "coverage", "stage", "stage_entity_type", "stage_entity_id", "step_number", "term_name", "factor_row_id", "slot_number", "variable", "policy_value", "matched_level")]
        match_trace_list[[length(match_trace_list) + 1L]] <- mt
      }
    }
  }

  if (!isTRUE(return_trace)) {
    return(current_value)
  }

  term_trace <- do.call(rbind, term_trace_list)
  rownames(term_trace) <- NULL
  factor_match_trace <- if (length(match_trace_list) > 0L) do.call(rbind, match_trace_list) else data.frame()
  if (nrow(factor_match_trace) > 0L) rownames(factor_match_trace) <- NULL

  out <- list(final_value = current_value, term_trace = term_trace)
  if (trace_detail %in% c("matches", "all")) {
    out$factor_match_trace <- factor_match_trace
  }
  out
}
