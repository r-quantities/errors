#' @rdname groupGeneric.errors
#'
#' @details \subsection{\code{Summary}}{
#' The methods \code{all} and \code{any} are not supported for \code{errors}
#' objects and fail with an informative message. \code{min}, \code{max} (and
#' \code{range}) return the minimum or (and) maximum value minus/plus its uncertainty.
#' \code{sum} and \code{prod} propagate the uncertainty as expected from the first-order
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
  dots <- list(...)
  dots[names(dots) != ""] <- NULL # unnamed only
  x <- do.call(c, dots)
  if (na.rm) x <- na.omit(x)
  switch(
    .Generic,
    "all" = , "any" =
      stop("method not supported for `errors` objects"),
    "sum" = set_errors(unclass(NextMethod()), sqrt(colSums(cbind(.e(x))^2))),
    "prod" = {
      xx <- NextMethod()
      set_errors(xx, sqrt(colSums(cbind(.e(x) * xx / .v(x))^2)))
    },
    "max" = max(errors_max(x)),
    "min" = min(errors_min(x)),
    "range" = range(errors_min(x), errors_max(x))
  )
}

#' Arithmetic Mean and Median Value
#'
#' S3 methods for \code{errors} objects.
#'
#' @param x an \code{errors} object.
#' @param ... further arguments passed to of from other methods.
#' @inheritParams base::mean
#'
#'
#' @details The \code{mean} and \code{weighted.mean} methods set the uncertainty as
#' the maximum of the standard deviation of the mean and the (weighted) mean of the uncertainty.
#'
#' The \code{median} method sets the uncertainty as \code{1.253 * errors(mean(x))},
#' which is derived from the asymptotic variance formula of the median. Note that
#' this value is valid only if the sample is big enough.
#'
#' @return An \code{errors} object.
#'
#' @export
mean.errors <- function(x, trim = 0, na.rm = FALSE, ...) {
  if (na.rm) x <- na.omit(x)
  e <- max(mean(.e(x)), sd(.v(x))/sqrt(length(x)))
  set_errors(unclass(NextMethod()), e)
}

#' @name mean.errors
#' @export
weighted.mean.errors <- function(x, ..., na.rm = FALSE) {
  if (na.rm) x <- na.omit(x)
  e <- max(weighted.mean(.e(x), ...), sd(.v(x))/sqrt(length(x)))
  set_errors(unclass(NextMethod()), e)
}

#' @name mean.errors
#' @export
median.errors = function(x, na.rm = FALSE, ...) {
  if (na.rm) x <- na.omit(x)
  e <- 1.253 * .e(mean(x))
  set_errors(unclass(NextMethod()), e)
}

#' @export
quantile.errors <- function(x, ...) {
  quantile(unclass(x), ...)
}

#' @export
summary.errors <- function(object, ...) {
  summary(unclass(object), ...)
}
