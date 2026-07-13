.stop_missing_cols <- function(x, cols, object_name = deparse(substitute(x))) {
  cols <- cols[!is.na(cols) & nzchar(as.character(cols))]
  missing <- setdiff(cols, names(x))
  if (length(missing) > 0) {
    stop(object_name, " is missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

.is_blank <- function(x) {
  is.null(x) || length(x) == 0 || (length(x) == 1 && (is.na(x) || !nzchar(as.character(x))))
}

.first_nonblank <- function(...) {
  vals <- list(...)
  for (v in vals) if (!.is_blank(v)) return(v)
  NA_character_
}

.get_scalar <- function(row, name, default = NA) {
  if (.is_blank(name) || !(name %in% names(row))) return(default)
  val <- row[[name]]
  if (length(val) == 0) return(default)
  val[[1]]
}

.add_default_col <- function(x, name, value) {
  if (!(name %in% names(x))) x[[name]] <- value
  x
}

.safe_numeric <- function(x, name = "value") {
  out <- suppressWarnings(as.numeric(x))
  if (length(out) != 1 || is.na(out)) stop(name, " must be a single numeric value.", call. = FALSE)
  out
}

.split_csv <- function(x) {
  if (.is_blank(x)) return(character(0))
  y <- unlist(strsplit(as.character(x), ",", fixed = TRUE), use.names = FALSE)
  trimws(y[nzchar(trimws(y))])
}

.combine_rows <- function(rows) {
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(data.frame())
  all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    for (nm in setdiff(all_names, names(x))) x[[nm]] <- NA
    x[all_names]
  })
  do.call(rbind, rows)
}
