#' @title R6 based key-value mapping
#'
#' @description
#' This class is basically a python dictionary.
#'
#' @export
Mapping <- R6::R6Class(
  classname = "Mapping",
  class = TRUE,
  portable = TRUE,
  cloneable = TRUE,
  public = list(
    #' @param ... Any length of key and value pairs a single list, named vector,
    #' or data.frame. If you would like to use a not valid R name as a key, you
    #' must wrap it by backquotes or convert it using \code{make.names}. If key
    #' value pairs are provided, they are are constructed using rlang::list2,
    #' thus the items are constructed sequentially.
    #'
    #' @examples
    #' ages <- Mapping$new(
    #'   Charlie = 40L,
    #'   Alice = 30L,
    #'   Bob = 25L
    #' )
    initialize = function(...) {
      if (...length() == 1) {
        x <- as.list(..1)
        # Only one object
        if (length(x) == sum(names(x) != "", na.rm = TRUE)) {
          # the object has all its names
          private$data <- collections::ordered_dict(x)
        } else {
          if (all(lengths(x) == 2)) {
            # list of two item lists
            x <- unname(unlist(x))
            private$data <- collections::ordered_dict(
              # select every other item
              x[c(FALSE, TRUE)],
              x[c(TRUE, FALSE)]
            )
          } else {
            stop("All items in a list or vector must be named.")
          }
        }
      } else {
        # we assume the dots should be constructed into a list,
        # which is done using the rlang list2 function.
        x <- rlang::list2(...)

        if (length(x) == sum(names(x) != "", na.rm = TRUE)) {
          # the object has all its names
          private$data <- collections::ordered_dict(x)
        } else {
          stop("All arguments must be named.")
        }
      }
    },

    #' @param key a string.
    #' @param value any value.
    set = function(key, value) {
      private$data$set(key, value) # invisible return
      return(NULL)
    },
    #' @param key a string.
    #' @param default any value.
    get = function(key, default = NULL) {
      private$data$get(key, default)
    },
    #' @param key a string.
    has = function(key) {
      private$data$has(key)
    },
    #' @param key a string.
    #' @param silent logical. Whether or not to show a message after removal.
    remove = function(key, silent = TRUE) {
      private$data$remove(key, silent) # invisible return
      return(NULL)
    },
    #' Return all the keys of a dictionary.
    keys = function() {
      private$data$keys()
    },
    #' Return all the values in the dictionary.
    values = function() {
      private$data$values()
    },
    #' Remove all key value pairs from the dictionary.
    clear = function() {
      private$data$clear()
    },
    #' Convert the dictionary to a list.
    as_list = function() {
      private$data$as_list()
    },
    #' @param inherits_mapping a subclass of Mapping
    update = function(inherits_mapping) {
      stopifnot(inherits(inherits_mapping, "Mapping"))
      private$data$update(inherits_mapping$.__data) # invisible return
      return(NULL)
    },
    #' @param key a string.
    #' @param default any value.
    pop = function(key, default = NULL) {
      private$data$pop(key, default)
    }
  ),
  active = list(
    #' Internal accessor for underlying data.
    .__data = function() {
      private$data
    },
    #' Very similar to as_list but the return value is a named list with the key and value.
    items = function() {
      lapply(self$keys(), function(k) list(key = k, value = self$get(k)))
    },
    #' The number of key-value pairs in the mapping.
    #' @return numeric.
    size = function() {
      private$data$size()
    }
  ),
  private = list(
    data = NULL
  )
)


#' @export
`[.Mapping` <- function(x, key = NULL, default = NULL) {
  x$get(key, default)
}


#' @export
`[<-.Mapping` <- function(x, key, value) {
  x$set(key, value)
  x
}


#' @export
length.Mapping <- function(x, ...) {
  x$size
}


#' @export
as.list.Mapping <- function(x, ...) {
  x$as_list()
}


#' @export
as.data.frame.Mapping <- function(x, ...) {
  if (length(unique(lengths(x$values()))) != 1) {
    stop("All values must be the same length.")
  }
  as.data.frame(as.list(x))
}


#' @export
as.matrix.Mapping <- function(x, ...) {
  as.matrix(as.data.frame(x))
}


#' @title R6 Based Key-Value Dictionary Implementation
#'
#' @description
#' A key-value dictionary data structure based on R6 class which is designed to
#' be similar usages with other languages dictionary (e.g. Python) with
#' reference semantics and extendabilities by R6.
#'
#' @examples
#' ages <- Dict$new(
#'   Charlie = 40L,
#'   Alice = 30L,
#'   Bob = 25L
#' )
#' @export
Dict <- R6::R6Class(
  classname = "Dict",
  inherit = Mapping,
  class = TRUE,
  portable = TRUE
)


#' Dict Class Constructor
#'
#' @rdname Dict
#'
#' @examples
#' ages <- dict(
#'   Charlie = 40L,
#'   Alice = 30L,
#'   Bob = 25L
#' )
#'
#' @param ... Any length of key and value pairs. If you would like to use
#' a not valid R name as a key, you must wrap it by backquotes or convert it
#' using \code{\link{make.names}}.
#'
#' @return A \code{Dict} class object.
#'
#' @export
dict <- function(...) {
  Dict$new(...)
}
