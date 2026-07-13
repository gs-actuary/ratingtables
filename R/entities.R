#' Rate child/entity rows
#' @export
rate_entities <- function(entity_data, plan, validate = TRUE) rate_policies_with_trace(entity_data, plan, validate = validate)

#' Backward-compatible entity scoring wrapper
#' @export
score_entity_rows <- function(entity_data, plan, validate = TRUE) rate_entities(entity_data, plan, validate = validate)$rated_data

#' Aggregate rated entity values to parent/group rows
#' @export
aggregate_entity_values <- function(rated_entity_data, group_col, value_cols, aggregation = "mean", weight_col = NULL, output_names = NULL, output_prefix = NULL) {
  d <- as.data.frame(rated_entity_data, stringsAsFactors = FALSE)
  .stop_missing_cols(d, c(group_col, value_cols), "rated_entity_data")
  groups <- unique(d[[group_col]])
  out <- data.frame(group_value = groups, stringsAsFactors = FALSE); names(out)[1] <- group_col
  if (is.null(output_names)) {
    if (is.null(output_prefix)) output_prefix <- paste0(aggregation, "_")
    output_names <- paste0(output_prefix, value_cols)
  }
  if (length(output_names) != length(value_cols)) stop("output_names must have same length as value_cols.", call. = FALSE)
  for (j in seq_along(value_cols)) {
    vals <- numeric(length(groups))
    for (i in seq_along(groups)) {
      sub <- d[d[[group_col]] == groups[[i]], , drop = FALSE]
      x <- as.numeric(sub[[value_cols[[j]]]])
      if (aggregation == "sum") vals[i] <- sum(x, na.rm = TRUE)
      else if (aggregation == "mean") vals[i] <- mean(x, na.rm = TRUE)
      else if (aggregation == "min") vals[i] <- min(x, na.rm = TRUE)
      else if (aggregation == "max") vals[i] <- max(x, na.rm = TRUE)
      else if (aggregation == "count") vals[i] <- sum(!is.na(x))
      else if (aggregation == "weighted_mean") {
        if (is.null(weight_col)) stop("weighted_mean requires weight_col.", call. = FALSE)
        w <- as.numeric(sub[[weight_col]])
        vals[i] <- if (sum(w, na.rm = TRUE) == 0) NA_real_ else stats::weighted.mean(x, w, na.rm = TRUE)
      } else stop("Unsupported aggregation: ", aggregation, call. = FALSE)
    }
    out[[output_names[[j]]]] <- vals
  }
  out
}

#' Average entity factors
#' @export
average_entity_factors <- function(scored_entity_data, group_col, coverages, output_prefix = "avg_entity_factor_") {
  value_cols <- paste0("indicated_", coverages)
  output_names <- paste0(output_prefix, coverages)
  aggregate_entity_values(scored_entity_data, group_col, value_cols, "mean", output_names = output_names)
}

#' Join aggregated entity values back to parent rows
#' @export
join_entity_values <- function(parent_data, entity_values, by) merge(as.data.frame(parent_data, stringsAsFactors = FALSE), as.data.frame(entity_values, stringsAsFactors = FALSE), by = by, all.x = TRUE, sort = FALSE)

#' Backward-compatible join wrapper
#' @export
join_entity_factors <- function(rating_data, entity_factor_data, by) join_entity_values(rating_data, entity_factor_data, by)
