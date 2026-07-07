# Internal helpers ---------------------------------------------------------

.is_blank <- function(x) {
  is.null(x) || length(x) == 0L || is.na(x) || identical(x, "")
}

.empty_to_na <- function(x) {
  x[x == ""] <- NA
  x
}

.required_slot_cols <- function(max_vars = 12L) {
  out <- character(2L * max_vars)
  j <- 1L
  for (i in seq_len(max_vars)) {
    out[[j]] <- paste0("variable", i)
    out[[j + 1L]] <- paste0("level", i)
    j <- j + 2L
  }
  out
}

.count_populated_slots <- function(one_row, max_vars = 12L) {
  out <- 0L
  for (i in seq_len(max_vars)) {
    var_name <- one_row[[paste0("variable", i)]]
    if (!is.null(var_name) && length(var_name) > 0L && !is.na(var_name) && var_name != "") {
      out <- out + 1L
    }
  }
  out
}

.slot_variables <- function(factor_table, max_vars = 12L) {
  vars <- character()
  for (i in seq_len(max_vars)) {
    col <- paste0("variable", i)
    if (col %in% names(factor_table)) {
      vals <- as.character(factor_table[[col]])
      vals <- vals[!is.na(vals) & vals != ""]
      vars <- c(vars, vals)
    }
  }
  unique(vars)
}

.normalize_policy_row <- function(policy_row) {
  if (is.data.frame(policy_row)) {
    policy_row <- as.list(policy_row[1L, , drop = FALSE])
  }
  policy_row
}

.get_policy_value <- function(policy_row, name) {
  x <- policy_row[[name]]
  if (length(x) == 0L) {
    return(NULL)
  }
  x[[1L]]
}

.as_date_if_possible <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  as.Date(x)
}

.safe_policy_id <- function(policies, row_i, policy_id_col = "policy_id") {
  if (!is.null(policy_id_col) && policy_id_col %in% names(policies)) {
    return(policies[[policy_id_col]][[row_i]])
  }
  NA
}

.required_policy_columns <- function(plan) {
  required <- character()
  if (isTRUE(plan$use_rate_set_key)) {
    required <- c(required, "rate_set_key")
  } else {
    required <- c(required, "state", "charter", "book_segment", "rating_date")
  }
  required <- c(required, .slot_variables(plan$factor_table, plan$max_vars))
  continuous_terms <- plan$rating_spec$calculation_type %in% c("continuous_additive", "continuous_multiplicative")
  if (any(continuous_terms)) {
    required <- c(required, as.character(plan$rating_spec$continuous_var[continuous_terms]))
  }
  required <- required[!is.na(required) & required != ""]
  unique(required)
}

.stop_missing_cols <- function(x, required, object_name) {
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    stop(
      object_name, " is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.allowed_calculation_types <- function() {
  c("multiplicative", "additive", "continuous_additive", "continuous_multiplicative")
}

.allowed_rounding_rules <- function() {
  c("nearest_cent", "nearest_dollar", "up_dollar", "down_dollar", "digits", "nearest_increment", "nearest_dime")
}

.merge_preserve_order <- function(x, y, by, all_x = TRUE) {
  x$.ratingtable_order <- seq_len(nrow(x))
  out <- merge(x, y, by = by, all.x = all_x, sort = FALSE)
  out <- out[order(out$.ratingtable_order), , drop = FALSE]
  out$.ratingtable_order <- NULL
  rownames(out) <- NULL
  out
}

ensure_slot_columns <- function(factor_table, max_vars = 12) {
  for (i in seq_len(max_vars)) {
    var_col <- paste0("variable", i)
    lev_col <- paste0("level", i)

    if (!(var_col %in% names(factor_table))) {
      factor_table[[var_col]] <- NA_character_
    }

    if (!(lev_col %in% names(factor_table))) {
      factor_table[[lev_col]] <- NA_character_
    }
  }

  factor_table
}
