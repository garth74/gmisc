#########################################################################
######### String Interpolation
#########################################################################

#' @export
#' @inheritParams stringr::str_glue
str_format <- function(..., .sep = "", .envir = parent.frame()) {
  glue::glue(..., .sep = .sep, .envir = .envir)
}

#' @export
#' @inheritParams stringr::str_glue
str_format_map <- function(..., .data, .sep = "", .envir = parent.frame(), .na = "NA") {
  glue::glue_data(.data, ..., .sep = .sep, .envir = .envir, .na = .na)
}

#########################################################################
######### Whitespace relevant
#########################################################################

#' Remove ``chars`` from the left end of a string. If ``chars`` is NULL,
#' this function removes whitespace.
#'
#' @export
str_lstrip <- function(string, chars = NULL) {
  chars <- if (is.null(chars)) "\\s" else chars
  stopifnot(inherits(chars, "character"))
  pattern <- stringi::stri_sprintf("^[%s]+", str_join("", chars))
  stringi::stri_replace_all_regex(string, pattern, "")
}

#' Remove ``chars`` from the right end of a string. If ``chars`` is NULL,
#' this function removes whitespace.
#'
#' @export
str_rstrip <- function(string, chars = NULL) {
  chars <- if (is.null(chars)) "\\s" else chars
  stopifnot(inherits(chars, "character"))
  pattern <- stringi::stri_sprintf("[%s]+$", str_join("", chars))
  stringi::stri_replace_all_regex(string, pattern, "")
}

#' Remove ``chars`` from both ends of a string. If ``chars`` is NULL,
#' this function removes whitespace.
#'
#' @export
str_strip <- function(string, chars = NULL) {
  chars <- if (is.null(chars)) "\\s" else chars
  stopifnot(inherits(chars, "character"))
  pattern <- stringi::stri_sprintf("^[%1$s]+|[%1$s]+$", str_join("", chars))
  stringi::stri_replace_all_regex(string, pattern, "")
}

#########################################################################
######### Case Related
#########################################################################

#' Convert a string to title case.
#'
#' @export
#' @inheritParams stringr::str_to_title
str_title <- function(string) {
  stringi::stri_trans_totitle(string)
}

#' Convert a string to upper case.
#'
#' @export
#' @inheritParams stringr::str_to_upper
str_upper <- function(string) {
  stringi::stri_trans_toupper(string)
}

#' Convert a string to lower case.
#'
#' @export
#' @inheritParams stringr::str_to_lower
str_lower <- function(string) {
  stringi::stri_trans_tolower(string)
}

#' Convert a string to sentence case.
#'
#' @export
#' @inheritParams stringr::str_to_sentence
str_sentence <- function(string, locale = "en") {
  opts <- stringi::stri_opts_brkiter(type = "sentence")
  stringi::stri_trans_totitle(string, opts_brkiter = opts)
}

#' @export
str_casefold <- function(string) {
  stringi::stri_trans_casefold(string)
}

#########################################################################
######### Boolean
#########################################################################

#' Check whether a string contains the substring.
#'
#' @export
str_contains <- function(string, substring) {
  stringi::stri_detect_fixed(string, substring)
}

#' Check whether a string starts with ``prefix``.
#'
#' @export
str_startswith <- function(string, prefix) {
  stringi::stri_startswith_fixed(string, prefix)
}

#' Check whether a string ends with ``suffix``.
#'
#' @export
str_endswith <- function(string, suffix) {
  stringi::stri_endswith_fixed(string, suffix)
}

.str_check <- function(pattern, string) {
  !any(is.na(re_fullmatch(pattern, string)))
}

#' Check whether ``string`` only contains uppercase letters.
#'
#' @export
str_isupper <- function(string) {
  stopifnot(inherits(string, "character"))
  all(!nzchar(stringi::stri_replace_all_regex(string, "[A-Z]+", "")))
}

#' Check whether ``string`` only contains lowercase letters.
#'
#' @export
str_islower <- function(string) {
  stopifnot(inherits(string, "character"))
  all(!nzchar(stringi::stri_replace_all_regex(string, "[a-z]+", "")))
}

#' Check whether ``string`` only contains whitespace characters.
#'
#' @export
str_isspace <- function(string) {
  stopifnot(inherits(string, "character"))
  all(!nzchar(stringi::stri_replace_all_regex(string, "[\\s]+", "")))
}

#' Check whether ``string`` only contains digits.
#'
#' @export
str_isdigit <- function(string) {
  stopifnot(inherits(string, "character"))
  all(!nzchar(stringi::stri_replace_all_regex(string, "[0-9]+", "")))
}

#' Check whether ``string`` only contains letters.
#'
#' @export
str_isalpha <- function(string) {
  stopifnot(inherits(string, "character"))
  all(!nzchar(stringi::stri_replace_all_regex(string, "[a-zA-Z]+", "")))
}

#' Check whether ``string`` only contains letters and numbers.
#'
#' @export
str_isalnum <- function(string) {
  stopifnot(inherits(string, "character"))
  all(!nzchar(stringi::stri_replace_all_regex(string, "[a-zA-Z0-9]+", "")))
}

#' Check whether ``string`` only contains punctuation.
#'
#' @export
str_ispunct <- function(string) {
  stopifnot(inherits(string, "character"))
  all(!nzchar(stringi::stri_replace_all_regex(string, "[:punct:]+", "")))
}

#' Check whether ``string`` only contains punctuation, letters, and numbers.
#'
#' @export
str_isgraph <- function(string) {
  stopifnot(inherits(string, "character"))
  all(!nzchar(stringi::stri_replace_all_regex(string, "[:graph:]+", "")))
}

#########################################################################
######### Substrings & replacing
#########################################################################


#' Replace all occurences of ``substring`` with ``replacement`` in ``string``.
#'
#' @export
str_replace <- function(string, substring, replacement) {
  stringi::stri_replace_all_fixed(
    string, substring, replacement
  )
}

#' Same as str_replace, but ... is named arguments where each name is
#' a substring and its value is the replacement.
#'
#' @export
str_replace_map <- function(.string, ...) {
  params <- rlang::list2(...)
  checkmate::assert_named(params)
  replacements <- stats::setNames(
    unlist(params, use.names = FALSE),
    map(names(params), re_escape)
  )
  stringr::str_replace_all(.string, replacements)
}

#' Remove a prefix from a string.
#'
#' @export
str_removeprefix <- function(string, prefix) {
  stringi::stri_replace_all_regex(
    string, sprintf("^%s", prefix), ""
  )
}

#' Remove a suffix from a string.
#'
#' @export
str_removesuffix <- function(string, suffix) {
  stringi::stri_replace_all_regex(
    string, sprintf("%s$", suffix), ""
  )
}

#' Replace each ``\t`` with spaces.
#'
#' @export
str_expandtabs <- function(string, tabsize = 4) {
  stringi::stri_replace_all_regex(
    string, "\\t", str_rep(" ", tabsize)
  )
}

#' Find the first occurence of ``substring``.
#'
#' @export
str_find <- function(string, substring) {
  stopifnot(all(lengths(string) == 1))
  as.integer(
    stringi::stri_locate_first_fixed(
      string, substring, get_length = TRUE
    )[, "start"]
  )
}

#' Find the last occurence of ``substring``.
#'
#' @export
str_rfind <- function(string, substring) {
  stopifnot(all(lengths(string) == 1))
  as.integer(
    stringi::stri_locate_last_fixed(
      string, substring, get_length = TRUE
    )[, "start"]
  )
}

#########################################################################
######### Miscellaneous
#########################################################################


#' Create a new string by repeating ``string`` ``times``.
#'
#' @export
str_rep <- function(string, times) {
  stringi::stri_dup(string, times)
}

#' Count the number of times ``substring`` occurs within ``string``.
#'
#' @export
#' @inheritParams stringr::str_count
str_count <- function(string, substring) {
  stringi::stri_count_fixed(string, substring)
}

#' Count the number of times a regular expression occurs in ``string``.
#'
#' @export
#' @inheritDotParams stringr::regex
str_count_regex <- function(string, pattern, ...) {
  pattern <- stringr::regex(pattern, ...)
  stringr::str_count(string, pattern = pattern)
}

#' Split ``string`` on ``char``. Passing a value for ``n``
#' determines the number of splits to apply.
#'
#' @export
str_split <- function(string, char = " ", n = -1L) {
  stringi::stri_split_fixed(string, pattern = char, n = n)
}

#' Split ``string`` on newline characters.
#'
#' @export
str_splitlines <- function(string, omit_empty = TRUE) {
  stringi::stri_split_lines(string, omit_empty = omit_empty)
}

#' Join multiple strings into a single string
#'
#' If a single object is passed in after the separator,
#' it's assumed the user wants to flatten.
#' If several objects are passed, it's assumed that the
#' user wants to concatenate.
#'
#' @export
#' @param sep string inserted between each character
str_join <- function(sep, ...) {
  stopifnot(length(sep) == 1)
  if (...length() == 1 && inherits(..1, "list")) {
    # flatten
    return(stringi::stri_join(unlist(..1), collapse = sep))
  }
  # concatenate
  return(stringi::stri_join(..., collapse = sep))
}

#' @export
str_length <- function(string) {
  stringi::stri_length(string)
}

#########################################################################
######### Text wrapping
#########################################################################


#' Wrap strings into nicely formatted paragraphs
#'
#' This is a wrapper around [stringi::stri_wrap()] which implements
#' the Knuth-Plass paragraph wrapping algorithm.
#'
#' @param string character vector of strings to reformat.
#' @param width positive integer giving target line width in characters. A
#'   width less than or equal to 1 will put each word on its own line.
#' @param initial_indent non-negative integer giving indentation of first line in
#'  each paragraph
#' @param subsequent_indent non-negative integer giving indentation of following lines in
#'  each paragraph
#' @param break_long_words If `FALSE`, the default, wrapping will only occur at
#'   whitespace. If `TRUE`, can break on any non-word character (e.g. `/`, `-`).
#' @param simplify If `TRUE` output is concatenated with newlines.
#' @return A character vector of re-wrapped strings.
#' @export
str_textwrap <- function(string,
                         width = 70,
                         initial_indent = 0,
                         subsequent_indent = 0,
                         break_long_words = FALSE,
                         simplify = FALSE) {
  out <- stringi::stri_wrap(string,
    width = width, indent = initial_indent, exdent = subsequent_indent,
    whitespace_only = !break_long_words, simplify = FALSE
  )
  if (isTRUE(simplify)) {
    out <- sapply(out, stringi::stri_c, collapse = "\n")
  }
  out
}
