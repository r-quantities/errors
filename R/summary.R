#' @rdname groupGeneric.errors
#'
#' @details \subsection{\code{Summary}}{
#' The methods \code{all} and \code{any} are not supported for \code{errors}
#' objects and fail with an informative message. \code{min}, \code{max} (and
#' \code{range}) return the minimum or (and) maximum value minus/plus its error.
#' \code{sum} and \code{prod} propagate the error as expected from the first-order
#' Taylor series method.}
#'
#' @examples
#' c(min(x), max(x))
#' range(x)
#' sum(y)
#' prod(y)
#'
#' @export
Summary.errors <- function(..., na.rm = FALSE) {
  x <- c(...)
  switch(
    .Generic,
    "all" = , "any" =
      stop("method not supported for `errors` objects"),
    "sum" = structure(NextMethod(), "errors" = propagate(cbind(errors(x))), class = "errors"),
    "prod" = {
      value <- NextMethod()
      e <- propagate(cbind(errors(x) * value / .v(x)))
      structure(value, "errors" = e, class = "errors")
    },
    "max" = NextMethod() + errors(x)[which.max(x)],
    "min" = NextMethod() - errors(x)[which.min(x)],
    "range" = c(min(x), max(x))
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
