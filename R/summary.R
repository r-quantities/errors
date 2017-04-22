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

#' @export
mean.errors <- function(x, ...) {
  err <- max(mean(errors(x)), sd(.v(x))/sqrt(length(x)))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
weighted.mean.errors <- function(x, w, ...) {
  err <- max(weighted.mean(errors(x), w), sd(.v(x))/sqrt(length(x)))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
median.errors = function(x, na.rm = FALSE, ...) {
}

median.errors <- if (is.na(match("...", names(formals(median))))) {
  function(x, na.rm = FALSE) set_errors(median(.v(x)), 1.253 * errors(mean(x)))
} else {
  function(x, na.rm = FALSE, ...) set_errors(median(.v(x)), 1.253 * errors(mean(x)))
}

#' @export
quantile.errors <- function(x, ...) {
  quantile(unclass(x), ...)
}

#' @export
summary.errors <- function(object, ...) {
  summary(unclass(object), ...)
}
