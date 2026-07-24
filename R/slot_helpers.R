.normalize_max_vars <- function(max_vars = 12) {
  if (is.null(max_vars) || length(max_vars) == 0 || is.na(max_vars[1])) return(12L)
  out <- suppressWarnings(as.integer(max_vars[1]))
  if (is.na(out) || out < 0) stop("max_vars must be a non-negative integer.", call. = FALSE)
  out
}

.slot_names <- function(max_vars = 12) {
  max_vars <- .normalize_max_vars(max_vars)
  list(variables = paste0("variable", seq_len(max_vars)), levels = paste0("level", seq_len(max_vars)))
}

#' Add empty variable/level slot columns
#' @param n Number of rows.
#' @param max_vars Number of variable/level slots.
#' @examples
#' slots <- make_empty_slots(
#'   n = 2,
#'   max_vars = 3
#' )
#'
#' slots
#' @export
make_empty_slots <- function(n = 1, max_vars = 12) {
  max_vars <- .normalize_max_vars(max_vars)
  out <- data.frame(row_id = seq_len(n))
  for (i in seq_len(max_vars)) {
    out[[paste0("variable", i)]] <- NA_character_
    out[[paste0("level", i)]] <- NA_character_
  }
  out$row_id <- NULL
  out
}

#' Ensure variable/level slot columns exist
#' @param x Data frame.
#' @param max_vars Number of variable/level slots.
#' @examples
#' factors <- data.frame(
#'   term_name = "territory",
#'   term_value = 1.10
#' )
#'
#' factors <- ensure_slot_columns(
#'   factors,
#'   max_vars = 2
#' )
#'
#' factors
#' @export
ensure_slot_columns <- function(x, max_vars = 12) {
  max_vars <- .normalize_max_vars(max_vars)
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  for (i in seq_len(max_vars)) {
    vn <- paste0("variable", i); ln <- paste0("level", i)
    if (!(vn %in% names(out))) out[[vn]] <- NA_character_
    if (!(ln %in% names(out))) out[[ln]] <- NA_character_
  }
  out
}

#' Populate variable/level slots from named values
#' @param x Data frame.
#' @param ... Named vectors/lists of variable names and levels.
#' @param max_vars Maximum slot count.
#' @examples
#' factors <- data.frame(
#'   term_name = c("territory_limit", "territory_limit"),
#'   term_value = c(1.10, 0.95)
#' )
#'
#' factors <- populate_slots(
#'   factors,
#'   territory = c("A", "B"),
#'   limit = "100/300",
#'   max_vars = 2
#' )
#'
#' factors
#' @export
populate_slots <- function(x, ..., max_vars = 12) {
  max_vars <- .normalize_max_vars(max_vars)
  out <- ensure_slot_columns(x, max_vars)
  vals <- list(...)
  if (length(vals) == 0) return(out)
  if (length(vals) > max_vars) stop("Too many slot values for max_vars.", call. = FALSE)
  n <- nrow(out)
  for (i in seq_along(vals)) {
    nm <- names(vals)[[i]]
    if (.is_blank(nm)) stop("All supplied slot values must be named.", call. = FALSE)
    val <- vals[[i]]
    if (length(val) == 1) val <- rep(val, n)
    if (length(val) != n) stop("Slot value length must be 1 or nrow(x).", call. = FALSE)
    out[[paste0("variable", i)]] <- nm
    out[[paste0("level", i)]] <- as.character(val)
  }
  out
}
