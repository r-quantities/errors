# nocov start
#' @export
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
