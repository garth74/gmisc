#########################################################################
######### String Interpolation
#########################################################################

#' @export
#' @inheritParams stringr::str_glue
str_format <- function(..., .sep = "", .envir = parent.frame()) {
  stringr::str_glue(..., .sep = .sep, .envir = .envir)
}

#' @export
#' @inheritParams stringr::str_glue
str_format_map <- function(..., .data, .sep = "", .envir = parent.frame(), .na = "NA") {
  if (missing(.data)) stop("`data` must be provided as a named argument.")
  stringr::str_glue_data(.x = .data, ..., .sep = .sep, .envir = .envir, .na = .na)
}

#########################################################################
######### Whitespace relevant
#########################################################################

#' Remove ``chars`` from the left end of a string. If ``chars`` is NULL,
#' this function removes whitespace.
#'
#' @export
str_lstrip <- function(string, chars = NULL) {
  if (is.null(chars)) {
    # trim whitespace
    return(stringr::str_trim(string, "left"))
  }
  # trim given characters
  chars <- re_escape(str_join("", chars))
  pattern <- stringr::regex(str_join("", c("^[", chars, "]")))
  stringr::str_replace_all(string, pattern, "")
}

#' Remove ``chars`` from the right end of a string. If ``chars`` is NULL,
#' this function removes whitespace.
#'
#' @export
str_rstrip <- function(string, chars = NULL) {
  if (is.null(chars)) {
    # trim whitespace
    return(stringr::str_trim(string, "right"))
  }
  # trim given characters
  chars <- stringi::stri_escape_unicode(str_join("", chars))
  pattern <- stringr::regex(str_join("", c("[", chars, "]$")))
  stringr::str_replace(string, pattern, "")
}

#' Remove ``chars`` from both ends of a string. If ``chars`` is NULL,
#' this function removes whitespace.
#'
#' @export
str_strip <- function(string, chars = NULL) {
  if (is.null(chars)) {
    # trim whitespace
    return(stringr::str_trim(string, "both"))
  }
  # trim given characters
  str_rstrip(str_lstrip(string, chars), chars)
}

#' Simplify redundant whitespace characters.
#'
#' @export
#' @inheritParams stringr::str_squish
str_normalize_whitespace <- function(string) {
  stringr::str_squish(string)
}

#########################################################################
######### Case Related
#########################################################################

#' Convert a string to title case.
#'
#' @export
#' @inheritParams stringr::str_to_title
str_title <- function(string, locale = "en") {
  stringr::str_to_title(string, locale = locale)
}

#' Convert a string to upper case.
#'
#' @export
#' @inheritParams stringr::str_to_upper
str_upper <- function(string, locale = "en") {
  stringr::str_to_upper(string, locale = locale)
}

#' Convert a string to lower case.
#'
#' @export
#' @inheritParams stringr::str_to_lower
str_lower <- function(string, locale = "en") {
  stringr::str_to_lower(string, locale = locale)
}

#' Convert a string to sentence case.
#'
#' @export
#' @inheritParams stringr::str_to_sentence
str_sentence <- function(string, locale = "en") {
  stringr::str_to_sentence(string, locale = locale)
}

#########################################################################
######### Boolean
#########################################################################

#' Check whether a string contains the substring.
#'
#' @export
str_contains <- function(string, substring, negate = FALSE, ignore_case = FALSE) {
  substring <- stringr::fixed(substring, ignore_case = ignore_case)
  stringr::str_detect(string, substring, negate = negate)
}

#' Check whether a string starts with ``prefix``.
#'
#' @export
str_startswith <- function(string, prefix, negate = FALSE, ignore_case = FALSE) {
  prefix <- stringr::fixed(prefix, ignore_case = ignore_case)
  stringr::str_starts(string, prefix, negate = negate)
}

#' Check whether a string ends with ``suffix``.
#'
#' @export
str_endswith <- function(string, suffix, negate = FALSE, ignore_case = FALSE) {
  suffix <- stringr::fixed(suffix, ignore_case = ignore_case)
  stringr::str_ends(suffix, negate = negate)
}

.str_check <- function(pattern, string) {
  !any(is.na(re_fullmatch(pattern, string)))
}

#' Check whether ``string`` only contains uppercase letters.
#'
#' @export
str_isupper <- function(string) {
  .str_check("[:upper:]+", string)
}

#' Check whether ``string`` only contains lowercase letters.
#'
#' @export
str_islower <- function(string) {
  .str_check("[:lower:]+", string)
}

#' Check whether ``string`` only contains whitespace characters.
#'
#' @export
str_isspace <- function(string) {
  .str_check("[:space:]+", string)
}

#' Check whether ``string`` only contains digits.
#'
#' @export
str_isdigit <- function(string) {
  .str_check("[:digit:]+", string)
}

#' Check whether ``string`` only contains letters.
#'
#' @export
str_isalpha <- function(string) {
  .str_check("[:alpha:]+", string)
}

#' Check whether ``string`` only contains letters and numbers.
#'
#' @export
str_isalnum <- function(string) {
  .str_check("[:alnum:]+", string)
}

#' Check whether ``string`` only contains punctuation.
#'
#' @export
str_ispunct <- function(string) {
  .str_check("[:punct:]+")
}

#' Check whether ``string`` only contains punctuation, letters, and numbers.
#'
#' @export
str_isgraph <- function(string) {
  .str_check("[:graph:]+")
}

#########################################################################
######### Substrings & replacing
#########################################################################


#' Replace all occurences of ``substring`` with ``replacement`` in ``string``.
#'
#' @export
str_replace <- function(string, substring, replacement) {
  pattern <- stringr::fixed(substring)
  stringr::str_replace_all(string, pattern, replacement)
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
  pattern <- paste0("^", re_escape(prefix))
  stringr::str_replace_all(string, pattern, "")
}

#' Remove a suffix from a string.
#'
#' @export
str_removesuffix <- function(string, suffix) {
  pattern <- paste0(re_escape(suffix), "$")
  stringr::str_replace_all(string, pattern, "")
}

#' Replace each ``\t`` with spaces.
#'
#' @export
str_expandtabs <- function(string, tabsize = 4) {
  replacement <- str_rep(" ", tabsize)
  stringr::str_replace_all(string, "\\t", replacement)
}

#' Find the first occurence of ``substring``.
#'
#' @export
str_find <- function(string, substring) {
  pattern <- stringr::fixed(substring, ignore_case = FALSE)
  suppressWarnings(vapply(
    stringr::str_locate_all(string, pattern),
    function(x) min(x[, "start"]),
    numeric(1)
  ))
}

#' Find the last occurence of ``substring``.
#'
#' @export
str_rfind <- function(string, substring) {
  pattern <- stringr::fixed(substring, ignore_case = FALSE)
  suppressWarnings(vapply(
    stringr::str_locate_all(string, pattern),
    function(x) max(x[, "start"]),
    numeric(1)
  ))
}

#########################################################################
######### Miscellaneous
#########################################################################


#' Create a new string by repeating ``string`` ``times``.
#'
#' @export
str_rep <- function(string, times) {
  checkmate::qassert(string, "S1")
  checkmate::qassert(times, "N1[1,)")
  stringr::str_dup(string, times)
}

#' Count the number of times ``substring`` occurs within ``string``.
#'
#' @export
#' @inheritParams stringr::str_count
str_count <- function(string, substring) {
  pattern <- stringr::fixed(substring, ignore_case = FALSE)
  stringr::str_count(string, pattern)
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
str_split <- function(string, char = " ", n = Inf) {
  stringr::str_split_fixed(string, pattern = char, n = n)
}

#' Split ``string`` on newline characters ('\n').
#'
#' @export
str_splitlines <- function(string) {
  stringr::str_split_fixed(string, pattern = "\n", n = Inf)
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
  checkmate::qassert(sep, "S1")
  if (...length() == 1) {
    # flatten
    return(stringr::str_flatten(..1, collapse = sep))
  }
  # concatenate
  return(stringr::str_c(..., sep = sep))
}

#' @export
#' @importFrom stringr str_length
stringr::str_length

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
#' @return A character vector of re-wrapped strings.
#' @export
str_textwrap <- function(string,
                         width = 70,
                         initial_indent = 0,
                         subsequent_indent = 0,
                         break_long_words = FALSE) {
  checkmate::qassert(width, "N1()")
  checkmate::qassert(initial_indent, "N1()")
  checkmate::qassert(subsequent_indent, "N1()")
  checkmate::qassert(break_long_words, "B1")

  out <- stringi::stri_wrap(string,
    width = width, indent = initial_indent, exdent = subsequent_indent,
    whitespace_only = !break_long_words, simplify = FALSE
  )
  vapply(out, stringr::str_c, collapse = "\n", character(1))
}
