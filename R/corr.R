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

#' Handle Correlations Between \code{errors} Objects
#'
#' Set or retrieve covariances or correlations between \code{errors} objects.
#' See the details section below.
#'
#' @param x an object of class \code{errors}.
#' @param y an object of class \code{errors}.
#' @inheritParams errors
#'
#' @return \code{covar} and \code{correl} return a vector of covariances and
#' correlations respectively (or \code{NULL}).
#'
#' @details The uncertainties associated to \code{errors} objects are supposed
#' to be independent by default. If there is some known correlation, it can be
#' defined using these methods, and it will be used for the propagation of the
#' uncertainty by the mathematical and arithmetic operations.
#'
#' The \code{correl} method sets or retrieves correlations, i.e., a value (or
#' vector of values) between \code{-1} and \code{1} (see base \code{\link{cor}}
#' on how to compute correlations). A covariance is just a correlation value
#' multiplied by the standard deviations (i.e., the standard uncertainty) of
#' both variables. It can be defined using the \code{covar} method (see base
#' \code{\link{cov}} on how to compute covariances). These methods are
#' equivalent; in fact, \code{correl} calls \code{covar} internally.
#'
#' Every \code{errors} object has a unique ID, and pairwise correlations are
#' stored in an internal hash table. All the functions or methods that modify
#' somehow the dimensions of \code{errors} objects (i.e., subsets, binds,
#' concatenations, summaries...) generate new objects with new IDs, and
#' correlations are not, and cannot be, propagated. Only mathematical and
#' arithmetic operations propagate correlations, where appropriate, following
#' the Taylor series method.
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

#' @name covar
#' @export
correl <- function(x, y) UseMethod("correl")

#' @export
correl.errors <- function(x, y) {
  xx <- covar(x, y)
  if (is.null(xx)) xx
  else xx / errors(x) / errors(y)
}

#' @name covar
#' @export
`correl<-` <- function(x, y, value) UseMethod("correl<-")

#' @export
`correl<-.errors` <- function(x, y, value) {
  stopifnot(value >= -1, value <= 1)
  covar(x, y) <- value * errors(x) * errors(y)
  x
}
