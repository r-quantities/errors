# Hash table of covariances
covars <- new.env(parent = emptyenv())

#' @importFrom uuid UUIDgenerate
# Each UUID carries an associated environment with a finalizer registered.
# This environment acts as a reference counter: when all copies are removed,
# the finalizer is called by the GC and the correlations are cleaned up.
new_id <- function() {
  id <- UUIDgenerate()
  env <- new.env(parent = emptyenv())
  env[["id"]] <- id
  reg.finalizer(env, function(x) {
    if (x[["id"]] %in% ls(covars)) {
      for (var in ls(covars[[x[["id"]]]]))
        rm(list = x[["id"]], pos = covars[[var]])
      rm(list = x[["id"]], pos = covars)
    }
  })
  environment(id) <- env
  id
}

# Get a covariance; covars[[idy]][[idx]] would return the same result.
ids_covar <- function(idx, idy) covars[[idx]][[idy]]

# Store a covariance in the hash table.
`ids_covar<-` <- function(idx, idy, value) {
  if (is.null(covars[[idx]]))
    covars[[idx]] <- new.env(parent = emptyenv())
  if (is.null(covars[[idy]]))
    covars[[idy]] <- new.env(parent = emptyenv())

  covars[[idx]][[idy]] <- value
  covars[[idy]][[idx]] <- covars[[idx]][[idy]]
  idx
}

ids <- function(id) {
  if (id %in% ls(covars)) ls(covars[[id]])
  else NULL
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
#' \code{set_covar} and \code{set_correl}, which are pipe-friendly versions of
#' the setters, return the \code{x} object.
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
#' @examples
#' V   <- with(GUM.H.2, set_errors(mean(V),   sd(V)   / sqrt(length(V))))
#' I   <- with(GUM.H.2, set_errors(mean(I),   sd(I)   / sqrt(length(I))))
#' phi <- with(GUM.H.2, set_errors(mean(phi), sd(phi) / sqrt(length(phi))))
#'
#' correl(V, I)   <- with(GUM.H.2, cor(V, I))
#' correl(V, phi) <- with(GUM.H.2, cor(V, phi))
#' correl(I, phi) <- with(GUM.H.2, cor(I, phi))
#'
#' (R <- (V / I) * cos(phi))
#' (X <- (V / I) * sin(phi))
#' (Z <- (V / I))
#'
#' correl(R, X)
#' correl(R, Z)
#' correl(X, Z)
#'
#' @export
covar <- function(x, y) UseMethod("covar")

#' @export
covar.errors <- function(x, y) {
  stopifnot(inherits(y, "errors"))
  ids_covar(attr(x, "id"), attr(y, "id"))
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
  ids_covar(attr(x, "id"), attr(y, "id")) <- value
  x
}

#' @name covar
#' @export
set_covar <- function(x, y, value) UseMethod("set_covar")

#' @export
set_covar.errors <- function(x, y, value) {
  covar(x, y) <- value
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

#' @name covar
#' @export
set_correl <- function(x, y, value) UseMethod("set_correl")

#' @export
set_correl.errors <- function(x, y, value) {
  correl(x, y) <- value
  x
}
