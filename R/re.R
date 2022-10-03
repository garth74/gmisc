#' @export
#' @inheritParams stringr::regex
re_compile <- function(pattern, ignore_case = FALSE, multiline = FALSE, comments = FALSE, dotall = FALSE, ...) {
  stringr::regex(pattern, ignore_case = ignore_case, multiline = multiline, comments = comments, dotall = dotall, ...)
}

#' Roughly equivalent to re.escape from python
#'
#' @export
#' @inheritParams stringi::stri_escape_unicode
re_escape <- function(str) {
  stringi::stri_escape_unicode(str)
}

#' @export
#' @inheritDotParams stringr::regex
#' @inheritParams stringr::str_extract_all string
re_findall <- function(pattern, string, ...) {
  pattern <- if (is_regex(pattern)) pattern else re_compile(pattern, ...)
  stringr::str_extract_all(string, pattern, simplify = TRUE)
}

#' @export
#' @inheritDotParams stringr::regex
#' @inheritParams stringr::str_extract string
re_search <- function(pattern, string, ...) {
  pattern <- if (is_regex(pattern)) pattern else re_compile(pattern, ...)
  stringr::str_extract(string, pattern, simplify = TRUE)
}

#' @export
#' @inheritDotParams stringr::regex
#' @inheritParams stringr::str_split string n
re_split <- function(pattern, string, n = Inf, ...) {
  pattern <- if (is_regex(pattern)) pattern else re_compile(pattern, ...)
  stringr::str_split(string, pattern, n = n, simplify = TRUE)
}

#' @export
#' @inheritDotParams stringr::regex
#' @inheritParams stringr::str_replace_all string replacement
re_sub <- function(pattern, replacement, string, ...) {
  pattern <- if (is_regex(pattern)) pattern else re_compile(pattern, ...)
  stringr::str_replace_all(string, pattern, replacement)
}

#' @export
#' @inheritDotParams stringr::regex
#' @inheritParams stringr::str_extract string
re_match <- function(pattern, string, ...) {
  pattern <- if (is_regex(pattern)) as.character(pattern) else pattern
  stringr::str_extract(string, re_compile(paste0("^", pattern), ...))
}

#' @export
#' @inheritDotParams stringr::regex
#' @inheritParams stringr::str_extract string
re_fullmatch <- function(pattern, string, ...) {
  pattern <- if (is_regex(pattern)) as.character(pattern) else pattern
  stringr::str_extract(string, re_compile(paste0("^", pattern, "$"), ...))
}
