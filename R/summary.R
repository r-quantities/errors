#' @export
Summary.errors <- function(..., na.rm = FALSE) {
  x <- c(...)
  switch(
    .Generic,
    "all" = , "any" =
      stop("method not supported for `errors` objects"),
    "prod" = {
      value <- NextMethod()
      e <- propagate(cbind(errors(x) * value / .v(x)))
      set_errors(value, e)
    },
    {
      e <- switch(
        .Generic,
        "sum" = propagate(cbind(errors(x))),
        "max" = errors(x)[which.max(x)],
        "min" = errors(x)[which.min(x)],
        "range" = c(errors(min(x)), errors(max(x)))
      )
      structure(NextMethod(), "errors" = e, class = "errors")
    }
  )
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
  e <- max(mean(errors(x)), sd(.v(x))/sqrt(length(x)))
  structure(NextMethod(), "errors" = e, class = "errors")
}

#' @name mean.errors
#' @export
weighted.mean.errors <- function(x, ...) {
  e <- max(weighted.mean(errors(x), ...), sd(.v(x))/sqrt(length(x)))
  structure(NextMethod(), "errors" = e, class = "errors")
}

#' @name mean.errors
#' @export
median.errors = function(x, ...) {
  e <- 1.253 * errors(mean(x))
  structure(NextMethod(), "errors" = e, class = "errors")
}

#' @export
quantile.errors <- function(x, ...) {
  quantile(unclass(x), ...)
}

#' @export
summary.errors <- function(object, ...) {
  summary(unclass(object), ...)
}
