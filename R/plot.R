# nocov start
#' @export
plot.errors <- function(x, y, ...) {
  if (missing(y)) {
    y <- x
    x <- seq_along(x)
  }

  xlim <- c(errors_min(min(x)), errors_max(max(x)))
  ylim <- c(errors_min(min(y)), errors_max(max(y)))

  NextMethod(xlim=xlim, ylim=ylim)

  if (inherits(x, "errors"))
    graphics::segments(errors_min(x), y, errors_max(x), y)

  if (inherits(y, "errors"))
    graphics::segments(x, errors_min(y), x, errors_max(y))
}
# nocov end
