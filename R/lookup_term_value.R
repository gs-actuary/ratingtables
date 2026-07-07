#' Look Up One Rating Term Value
#'
#' Looks up the factor, fee, or coefficient for one policy row, one coverage,
#' and one rating term.
#'
#' @param policy_row A one-row data frame or named list representing one policy.
#' @param plan A `rating_plan` object.
#' @param term_name Rating term to look up.
#' @param coverage Coverage to rate.
#' @param return_match If `TRUE`, return lookup metadata in addition to the
#'   looked-up value.
#'
#' @return If `return_match = FALSE`, a numeric value. If `TRUE`, a list with
#'   `term_value`, `factor_row_id`, `rate_set_key`, and `match_trace`.
#' @export
#'
#' @examples
#' ex <- example_rating_plan()
#' lookup_term_value(ex$policies[1, ], ex$plan, "territory", "BI")
lookup_term_value <- function(policy_row, plan, term_name, coverage, return_match = FALSE) {
  if (!inherits(plan, "rating_plan")) {
    stop("plan must be a rating_plan object.", call. = FALSE)
  }
  policy_row <- .normalize_policy_row(policy_row)
  factor_table <- plan$factor_table
  factor_table$.factor_row_index <- seq_len(nrow(factor_table))

  candidates <- factor_table[factor_table$term_name == term_name & factor_table$coverage == coverage, , drop = FALSE]

  if (isTRUE(plan$use_rate_set_key)) {
    if (!("rate_set_key" %in% names(policy_row))) {
      stop("Policy row is missing rate_set_key but use_rate_set_key = TRUE.", call. = FALSE)
    }
    candidates <- candidates[candidates$rate_set_key == .get_policy_value(policy_row, "rate_set_key"), , drop = FALSE]
  } else {
    needed <- c("state", "charter", "book_segment", "rating_date")
    missing <- setdiff(needed, names(policy_row))
    if (length(missing) > 0L) {
      stop("Policy row is missing automatic rate-set column(s): ", paste(missing, collapse = ", "), call. = FALSE)
    }
    rating_date <- .as_date_if_possible(.get_policy_value(policy_row, "rating_date"))
    candidates <- candidates[
      candidates$state == .get_policy_value(policy_row, "state") &
        candidates$charter == .get_policy_value(policy_row, "charter") &
        candidates$book_segment == .get_policy_value(policy_row, "book_segment") &
        candidates$rate_eff_date <= rating_date &
        candidates$rate_exp_date >= rating_date,
      , drop = FALSE
    ]
  }

  if (nrow(candidates) == 0L) {
    stop("No candidate rows found for term '", term_name, "', coverage '", coverage, "'.", call. = FALSE)
  }

  match_flags <- rep(FALSE, nrow(candidates))
  match_details <- vector("list", nrow(candidates))

  for (r in seq_len(nrow(candidates))) {
    this_row_matches <- TRUE
    detail_rows <- list()

    for (i in seq_len(plan$max_vars)) {
      slot_var <- candidates[[paste0("variable", i)]][[r]]
      slot_level <- candidates[[paste0("level", i)]][[r]]

      if (is.na(slot_var) || slot_var == "") {
        next
      }

      if (!(slot_var %in% names(policy_row))) {
        stop("Policy row does not contain required variable '", slot_var, "' for term '", term_name, "'.", call. = FALSE)
      }

      policy_value <- .get_policy_value(policy_row, slot_var)
      policy_value_chr <- as.character(policy_value)
      slot_level_chr <- as.character(slot_level)

      detail_rows[[length(detail_rows) + 1L]] <- data.frame(
        slot_number = i,
        variable = slot_var,
        policy_value = policy_value_chr,
        matched_level = slot_level_chr,
        stringsAsFactors = FALSE
      )

      if (is.na(policy_value_chr) || policy_value_chr != slot_level_chr) {
        this_row_matches <- FALSE
        break
      }
    }

    match_flags[[r]] <- this_row_matches
    if (length(detail_rows) > 0L) {
      match_details[[r]] <- do.call(rbind, detail_rows)
    } else {
      match_details[[r]] <- data.frame(
        slot_number = integer(),
        variable = character(),
        policy_value = character(),
        matched_level = character(),
        stringsAsFactors = FALSE
      )
    }
  }

  matched <- candidates[match_flags, , drop = FALSE]
  matched_details <- match_details[match_flags]

  if (nrow(matched) == 0L) {
    stop("No matching factor row found for term '", term_name, "', coverage '", coverage, "'.", call. = FALSE)
  }

  if (nrow(matched) > 1L) {
    specificity <- integer(nrow(matched))
    for (i in seq_len(nrow(matched))) {
      specificity[[i]] <- .count_populated_slots(as.list(matched[i, , drop = FALSE]), max_vars = plan$max_vars)
    }
    keep <- specificity == max(specificity)
    matched <- matched[keep, , drop = FALSE]
    matched_details <- matched_details[keep]
  }

  if (nrow(matched) != 1L) {
    stop("Lookup for term '", term_name, "', coverage '", coverage, "' returned ", nrow(matched), " rows after matching. Expected exactly 1.", call. = FALSE)
  }

  term_value <- matched$term_value[[1L]]
  if (!isTRUE(return_match)) {
    return(term_value)
  }

  factor_row_id <- if ("factor_row_id" %in% names(matched)) matched$factor_row_id[[1L]] else matched$.factor_row_index[[1L]]
  match_trace <- matched_details[[1L]]
  if (nrow(match_trace) > 0L) {
    match_trace$factor_row_id <- factor_row_id
    match_trace$term_name <- term_name
    match_trace$coverage <- coverage
    match_trace <- match_trace[c("coverage", "term_name", "factor_row_id", "slot_number", "variable", "policy_value", "matched_level")]
  }

  list(
    term_value = term_value,
    factor_row_id = factor_row_id,
    rate_set_key = if ("rate_set_key" %in% names(matched)) matched$rate_set_key[[1L]] else NA_character_,
    match_trace = match_trace
  )
}
