#' X-Y plotting with errorbars
#'
#' Method for plotting \code{errors} objects.
#'
#' @param x object of class errors to plot along the x axis, or, if y is missing, along the y axis.
#' @param y object of class errors to plot along the y axis, or missing.
#' @param ... other parameters, passed on to \link{plot.default}.
#' @export
plot.errors <- function(x, y, ...) {
  NextMethod()

  if (missing(y)) {
    y <- x
    x <- seq_along(x)
  }

  if (inherits(x, "errors"))
    graphics::segments(errors_min(x), y, errors_max(x), y)

  if (inherits(y, "errors"))
    graphics::segments(x, errors_min(y), x, errors_max(y))
}
