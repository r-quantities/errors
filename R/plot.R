#' Generic X-Y Plotting
#'
#' S3 method for \code{errors} objects which automatically prints the error bars.
#'
#' @inheritParams graphics::plot
#' @param ... additional arguments (see \code{\link[graphics]{plot}}).
#'
#' @examples
#' cars <- as.matrix(cars)
#' cars <- as.data.frame(set_errors(cars, cars * 0.05))
#' plot(cars)
#'
#' @export
# nocov start
plot.errors <- function(x, y, ...) {
  if (missing(y)) {
    y <- x
    x <- seq_along(x)
  }

  NextMethod(xlim=range(x), ylim=range(y))

  if (inherits(x, "errors"))
    graphics::segments(errors_min(x), y, errors_max(x), y)

  if (inherits(y, "errors"))
    graphics::segments(x, errors_min(y), x, errors_max(y))
}
# nocov end
