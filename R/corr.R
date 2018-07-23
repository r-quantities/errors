covars <- new.env(parent = emptyenv())

get_covar <- function(idx, idy) covars[[idx]][[idy]]

set_covar <- function(idx, idy, value) {
  if (is.null(covars[[idx]]))
    covars[[idx]] <- new.env(parent = emptyenv())
  if (is.null(covars[[idy]]))
    covars[[idy]] <- new.env(parent = emptyenv())

  covars[[idx]][[idy]] <- value
  covars[[idy]][[idx]] <- covars[[idx]][[idy]]
}

#' Set Correlations Between \code{errors} objects
#'
#' Set/retrieve correlations.
#'
#' @inheritParams errors
#' @param y an object of class \code{errors}.
#'
#' @export
covar <- function(x, y) UseMethod("covar")

#' @export
covar.errors <- function(x, y) {
  stopifnot(inherits(y, "errors"))
  stopifnot(length(x) == length(y))
  stopifnot(attr(x, "id") != attr(y, "id"))

  get_covar(attr(x, "id"), attr(y, "id"))
}

#' @name covar
#' @export
`covar<-` <- function(x, y, value) UseMethod("covar<-")

#' @export
`covar<-.errors` <- function(x, y, value) {
  stopifnot(inherits(y, "errors"))
  stopifnot(length(x) == length(y))
  stopifnot(attr(x, "id") != attr(y, "id"))
  stopifnot(length(value) == length(x) || length(value) == 1L)

  if (length(value) == 1)
    value <- rep(value, length(x))
  value[!is.finite(x)] <- x[!is.finite(x)]
  set_covar(attr(x, "id"), attr(y, "id"), abs(value))
  x
}
