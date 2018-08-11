# Hash table of covariances
ht <- new.env(parent = emptyenv())

.id <- function(id) {
  if (is.environment(id))
    id <- format(id)
  id
}

# Each ID is a new empty environment with a finalizer registered.
# This environment acts as a reference counter: when all copies are removed,
# the finalizer is called by the GC and the correlations are cleaned up.
new_id <- function() {
  env <- new.env(parent = emptyenv())
  reg.finalizer(env, function(x) {
    id <- .id(x)
    if (id %in% ls(ht)) {
      for (var in ls(ht[[id]]))
        rm(list = id, pos = ht[[var]])
      rm(list = id, pos = ht)
    }
  })
  env
}

# Get a covariance; ht[[idy]][[idx]] would return the same result.
.covar <- function(idx, idy) ht[[.id(idx)]][[.id(idy)]]

# Store a covariance in the hash table.
`.covar<-` <- function(idx, idy, value) {
  ret <- idx
  idx <- .id(idx)
  idy <- .id(idy)

  if (is.null(ht[[idx]]))
    ht[[idx]] <- new.env(parent = emptyenv())
  if (is.null(ht[[idy]]))
    ht[[idy]] <- new.env(parent = emptyenv())

  ht[[idx]][[idy]] <- value
  ht[[idy]][[idx]] <- ht[[idx]][[idy]]

  ret
}

ids <- function(id) {
  id <- .id(id)
  if (id %in% ls(ht)) ls(ht[[id]])
  else NULL
}

#' Handle Correlations Between \code{errors} Objects
#'
#' Set or retrieve correlations or covariances between \code{errors} objects.
#' See the details section below.
#'
#' @param x an object of class \code{errors}.
#' @param y an object of class \code{errors}.
#' @inheritParams errors
#'
#' @return \code{correl} and \code{covar} return a vector of correlations and
#' covariances respectively (or \code{NULL}).
#' \code{set_correl} and \code{set_covar}, which are pipe-friendly versions of
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
#' x <- set_errors(1:5, 0.1)
#' y <- set_errors(1:5, 0.1)
#'
#' # Self-correlation is of course 1, and cannot be changed
#' correl(x, x)
#' \dontrun{
#' correl(x, x) <- 0.5}
#'
#' # Cross-correlation can be set, but must be a value between -1 and 1
#' correl(x, y)
#' \dontrun{
#' correl(x, y) <- 1.5}
#' correl(x, y) <- runif(length(x))
#' correl(x, y)
#' covar(x, y)
#'
#' @export
correl <- function(x, y) UseMethod("correl")

#' @export
correl.errors <- function(x, y) {
  xx <- covar(x, y)
  if (is.null(xx)) xx
  else xx / errors(x) / errors(y)
}

#' @name correl
#' @export
`correl<-` <- function(x, y, value) UseMethod("correl<-")

#' @export
`correl<-.errors` <- function(x, y, value) {
  stopifnot(value >= -1, value <= 1)
  covar(x, y) <- value * errors(x) * errors(y)
  x
}

#' @name correl
#' @export
set_correl <- function(x, y, value) UseMethod("set_correl")

#' @export
set_correl.errors <- function(x, y, value) {
  correl(x, y) <- value
  x
}

#' @name correl
#' @export
covar <- function(x, y) UseMethod("covar")

#' @export
covar.errors <- function(x, y) {
  stopifnot(inherits(y, "errors"))
  .covar(attr(x, "id"), attr(y, "id"))
}

#' @name correl
#' @export
`covar<-` <- function(x, y, value) UseMethod("covar<-")

#' @export
`covar<-.errors` <- function(x, y, value) {
  stopifnot(inherits(y, "errors"))
  stopifnot(length(x) == length(y))
  stopifnot(!identical(attr(x, "id"), attr(y, "id")))
  stopifnot(length(value) == length(x) || length(value) == 1L)
  stopifnot(value / errors(x) / errors(y) >= -1,
            value / errors(x) / errors(y) <= 1)

  if (length(value) == 1)
    value <- rep(value, length(x))
  .covar(attr(x, "id"), attr(y, "id")) <- value
  x
}

#' @name correl
#' @export
set_covar <- function(x, y, value) UseMethod("set_covar")

#' @export
set_covar.errors <- function(x, y, value) {
  covar(x, y) <- value
  x
}
