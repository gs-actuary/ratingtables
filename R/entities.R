#' Rate child or entity records
#'
#' Apply a rating plan to entity-level records such as drivers, vehicles,
#' boats, or scheduled items, returning both rated values and trace output.
#'
#' @param entity_data A data frame containing one row per entity to be rated.
#' @param plan A `rating_plan` object created by [new_rating_plan()].
#' @param validate Logical. If `TRUE`, validate the entity data before rating.
#'
#' @return A `rating_result` object containing `rated_data`, `term_trace`,
#'   and the rating plan.
#'
#' @export
rate_entities <- function(entity_data, plan, validate = TRUE) rate_policies_with_trace(entity_data, plan, validate = validate)

#' Score child or entity records
#'
#' Backward-compatible wrapper around [rate_entities()] that returns only
#' the rated entity data and omits the trace and plan components.
#'
#' @param entity_data A data frame containing one row per entity to be rated.
#' @param plan A `rating_plan` object created by [new_rating_plan()].
#' @param validate Logical. If `TRUE`, validate the entity data before rating.
#'
#' @return A data frame containing the original entity data and calculated
#'   indicated values.
#'
#' @export
score_entity_rows <- function(entity_data, plan, validate = TRUE) rate_entities(entity_data, plan, validate = validate)$rated_data

#' Aggregate rated entity values to parent records
#'
#' Aggregate one or more numeric values from entity-level records to a parent
#' or group level. This can be used, for example, to average driver factors or
#' sum premiums for boats or scheduled items.
#'
#' @param rated_entity_data A data frame containing rated entity records.
#' @param group_col A character string naming the column that identifies the
#'   parent or aggregation group.
#' @param value_cols A character vector naming the numeric columns to
#'   aggregate.
#' @param aggregation A character string specifying the aggregation method.
#'   Supported values are `"sum"`, `"mean"`, `"min"`, `"max"`, `"count"`,
#'   and `"weighted_mean"`.
#' @param weight_col An optional character string naming the weight column.
#'   Required when `aggregation = "weighted_mean"`.
#' @param output_names An optional character vector giving the names of the
#'   aggregated output columns. It must have the same length as `value_cols`.
#' @param output_prefix An optional character string prepended to generated
#'   output names when `output_names` is not supplied. By default, the
#'   aggregation name followed by an underscore is used.
#'
#' @return A data frame with one row per unique value of `group_col` and one
#'   aggregated column for each entry in `value_cols`.
#'
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

#' Average entity rating factors
#'
#' Average indicated coverage values across entity records belonging to the
#' same parent record.
#'
#' @param scored_entity_data A data frame containing scored entity records and
#'   columns named `indicated_<coverage>`.
#' @param group_col A character string naming the column that identifies the
#'   parent or aggregation group.
#' @param coverages A character vector of coverage names whose indicated values
#'   should be averaged.
#' @param output_prefix A character string prepended to the generated output
#'   column names.
#'
#' @return A data frame with one row per parent group and one average entity
#'   factor column for each requested coverage.
#'
#' @export
average_entity_factors <- function(scored_entity_data, group_col, coverages, output_prefix = "avg_entity_factor_") {
  value_cols <- paste0("indicated_", coverages)
  output_names <- paste0(output_prefix, coverages)
  aggregate_entity_values(scored_entity_data, group_col, value_cols, "mean", output_names = output_names)
}

#' Join aggregated entity values to parent records
#'
#' Left-join aggregated entity-level values back to the parent-level rating
#' data.
#'
#' @param parent_data A data frame containing parent-level records.
#' @param entity_values A data frame containing aggregated entity values.
#' @param by A character vector naming the column or columns used to join the
#'   two data frames.
#'
#' @return A data frame containing all rows from `parent_data` with matching
#'   columns from `entity_values`.
#'
#' @export
join_entity_values <- function(parent_data, entity_values, by) merge(as.data.frame(parent_data, stringsAsFactors = FALSE), as.data.frame(entity_values, stringsAsFactors = FALSE), by = by, all.x = TRUE, sort = FALSE)

#' Join entity factors to rating data
#'
#' Backward-compatible wrapper around [join_entity_values()] for joining
#' aggregated entity factors to parent-level rating data.
#'
#' @param rating_data A data frame containing the parent-level rating records.
#' @param entity_factor_data A data frame containing aggregated entity factors.
#' @param by A character vector naming the column or columns used to join the
#'   two data frames.
#'
#' @return A data frame containing all rows from `rating_data` with matching
#'   entity-factor columns appended.
#'
#' @export
join_entity_factors <- function(rating_data, entity_factor_data, by) join_entity_values(rating_data, entity_factor_data, by)
