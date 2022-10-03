is_regex <- function(x) {
  inherits(x, "regex")
}

is_true <- function(x) {
  isTRUE(x)
}

get_names <- function(x) {
  names(x)
}
