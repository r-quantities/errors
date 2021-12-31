#' Scatterplot with Error Bars
#'
#' S3 method for \code{errors} objects which automatically prints the error bars.
#'
#' @inheritParams graphics::plot
#' @param ... additional arguments (see \code{\link{plot}}).
#'
#' @examples
#' cars <- as.matrix(cars)
#' cars <- as.data.frame(set_errors(cars, cars * 0.05))
#' plot(cars$speed)
#' plot(cars)
#'
#' @export
plot.errors <- function(x, y, ...) {
  dots <- list(...)
  xlim <- dots$xlim
  ylim <- dots$ylim
  xlab <- dots$xlab
  ylab <- dots$ylab

  if (missing(y)) {
    if (is.null(ylim))
      ylim <- range(x[is.finite(x)])
    if (is.null(ylab))
      ylab <- deparse1(substitute(x))
  } else {
    if (is.null(xlim))
      xlim <- range(x[is.finite(x)])
    if (is.null(xlab))
      xlab <- deparse1(substitute(x))
    if (is.null(ylim))
      ylim <- range(y[is.finite(y)])
    if (is.null(ylab))
      ylab <- deparse1(substitute(y))
  }

  NextMethod(xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab)

  col <- if (!is.null(dots$col)) dots$col else graphics::par("fg")

  if (missing(y)) {
    xi <- seq_along(x)
    graphics::segments(xi, errors_min(x), xi, errors_max(x), col=col)
  } else {
    graphics::segments(errors_min(x), y, errors_max(x), y, col=col)
    graphics::segments(x, errors_min(y), x, errors_max(y), col=col)
  }
}
