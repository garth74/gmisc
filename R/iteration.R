#' @param ... Objects to be zipped together.
#'
#' @return a list of lists
#' @export
zip <- function(...) {
  mapply(list, ..., SIMPLIFY = FALSE)
}

#' Create a list with the index and values of each element passed into ...
#'
#' @param ... vectors or lists to be combined. They should be the same length.
#' @param .start numeric value. The first index value.
#' @param .step numeric value. Step size for each iteration.
#' @export
enumerate <- function(..., .start = 1, .step = 1) {
  ix <- (seq_along(..1) + .start - 1) + (seq_along(..1) * (.step - 1) - (.step - 1))
  zip(ix = ix, ...)
}

#' Split a vector into ``n`` size chunks.
#'
#' @param x vector.
#' @param n numeric value. Chunk size.
#' @export
chunk <- function(x, n) {
  split(x, cut(seq_along(x), n, labels = paste0("..", seq_len(n))))
}

#' Return only the values of a list that meet a certain predicate.
#'
#' @param x list. The list to filter.
#' @param f function. The functions should return TRUE or FALSE.
#' @export
filter <- function(x, f, negate = FALSE) {
  if (isFALSE(negate)) Filter(f, x) else Filter(Negate(f), x)
}

#' Paritition a list into two lists based on the reuslts of a binary function.
#'
#' @param x list. The list to partition.
#' @param f function. A function that returns TRUE or FALSE for each element
#'   into the list, determining which partition each element ends up in.
#' @export
partition <- function(x, f) {
  filtered <- Filter(f, x)
  list(filtered, setdiff(x, filtered))
}

#' Recursively recombine values of a list.
#'
#' @param x list. The list to reduce.
#' @param f function. The reducing function to apply, taking two values and
#'    producing one value.
#' @param init object. An optional initialization value to start reducing.
#' @export
reduce <- function(x, f, init) {
  Reduce(f, x, init)
}

#' Repeated apply the same function to `x`, using the output as the input for
#' each successive call.
#'
#' @param x any R object.
#' @param f function. The function should only accept one argument.
#' @param times a number.
reapply <- function(x, f, times) {
  Reduce(function(.x, .i) f(.x), seq_len(times), x)
}

#' Turn a list of lists into a list.
#' @param x list. The list of lists to unnest
#' @export
flatten <- function(x, depth = -1) {
  if (depth == -1) {
    return(as.list(unlist(x, recursive = TRUE)))
  }
  f <- function(.x, i) as.list(unlist(.x, recursive = FALSE))
  Reduce(f, seq_len(depth), x)
}

#' Iterate a function over a list.
#'
#' @param x list. The list-of-lists to iterate over.
#' @param f function. The function to apply.
#' @param ... list. Additional optional arguments to pass to lapply.
#' @export
map <- function(x, f, ..., .method = c("default", "names", "values")) {
  switch(match.arg(.method),
    "default" = lapply(x, f, ...),
    "names" = stats::setNames(x, lapply(names(x), f, ...)),
    "values" = unlist(lapply(unlist(x), f, ...))
  )
}

#' Partially apply a function
#'
#' Simplify a function by setting some arguments to pre-specified values
#'
#' @param f a function
#' @param ... arguments to capture
#'
#' @references \url{http://stackoverflow.com/questions/32173901/how-to-efficiently-partially-apply-a-function-in-r}, \url{https://stackoverflow.com/a/31900149/986793} #nolint
#'
#' @examples
#' # Example 1:
#' f <- function(a, b, c, d) a + b + c + d
#' p <- partial(f, a = 2, c = 3)
#' p(b = 0, d = 1)
#'
#' # captures a format string for printing out sleep data
#' labeller <- partial(sprintf, fmt = "extra=%3.2f, group=%d, ID=%d")
#' do.call(labeller, sleep[1, , drop = FALSE])
#'
#' @export
partial <- function(f, ...) {
  force(f)
  l <- rlang::list2(...)
  function(...) do.call(f, c(l, list(...)))
}
