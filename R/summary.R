#' @export
sum.errors <- function(..., na.rm = FALSE) {
  err <- propagate(cbind(errors(c(...))))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
prod.errors <- function(..., na.rm = FALSE) {
  value <- NextMethod()
  x <- c(...)
  err <- propagate(cbind(errors(x) * value / .v(x)))
  set_errors(value, err)
}

#' @export
max.errors <- function(..., na.rm = FALSE) {
  err <- errors(c(...))[which.max(c(...))]
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
min.errors <- function(..., na.rm = FALSE) {
  err <- errors(c(...))[which.min(c(...))]
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' Arithmetic Mean and Median Value
#'
#' S3 methods for \code{errors} objects.
#'
#' @param x an \code{errors} object.
#' @param ... further arguments passed to of from other methods.
#'
#'
#' @details The \code{mean} and \code{weighted.mean} methods set the error as
#' the maximum of the standard error of the mean and the (weighted) mean of the errors.
#'
#' The \code{median} method sets the error as \code{1.253 * errors(mean(x))},
#' which is derived from the asymptotic variance formula of the median. Note that
#' this value is valid only if the sample is big enough.
#'
#' @return An \code{errors} object.
#'
#' @export
mean.errors <- function(x, ...) {
  err <- max(mean(errors(x)), sd(.v(x))/sqrt(length(x)))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @name mean.errors
#' @export
weighted.mean.errors <- function(x, ...) {
  err <- max(weighted.mean(errors(x), ...), sd(.v(x))/sqrt(length(x)))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @name mean.errors
#' @export
median.errors = function(x, ...) {
  err <- 1.253 * errors(mean(x))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
quantile.errors <- function(x, ...) {
  quantile(unclass(x), ...)
}

#' @export
summary.errors <- function(object, ...) {
  summary(unclass(object), ...)
}
