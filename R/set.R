#' Get the values that intersection between x and all containers in ...
#' @export
set_intersection <- function(x, ..., .names_only = FALSE) {
  args <- if (is_true(.names_only)) lapply(list(x, ...), get_names) else list(x, ...)
  Reduce(intersect, args)
}

#' Get the union of x and ...
#' @export
set_union <- function(x, ..., .names_only = FALSE) {
  args <- if (is_true(.names_only)) lapply(list(x, ...), get_names) else list(x, ...)
  Reduce(union, args)
}

#' Get the values of x that only exist in x and not y.
#' @export
set_difference <- function(x, ..., .names_only = FALSE) {
  args <- if (is_true(.names_only)) lapply(list(x, ...), get_names) else list(x, ...)
  Reduce(setdiff, args)
}

#' Test whether x and y contain the same elements.
#' @export
set_equal <- function(x, y, .names_only = FALSE) {
  args <- if (isTRUE(.names_only)) lapply(list(x, y), get_names) else list(x, y)
  vapply(args, function(y) setequal(x, y), logical(1L), USE.NAMES = FALSE)
}

#' Test whether x is a subset of y.
#' @export
set_issubset <- function(x, y, .names_only = FALSE) {
  args <- if (isTRUE(.names_only)) lapply(list(x, y), get_names) else list(x, y)
  all(is.element(x, y))
}

#' Test whether y is a subset of x.
#' @export
set_issuperset <- function(x, y, .names_only = FALSE) {
  args <- if (isTRUE(.names_only)) lapply(list(x, y), get_names) else list(x, y)
  all(is.element(y, x))
}
