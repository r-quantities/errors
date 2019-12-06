#' Scatterplot with Error Bars
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
  call <- match.call()
  lcall <- length(call)
  dots <- list(...)

  if (missing(y)) {
    if (!exists("ylim", dots))
      call$ylim <- substitute(range(x[is.finite(x)]))
    if (!exists("ylab", dots))
      call$ylab <- deparse(substitute(x))
  } else {
    if (!exists("xlim", dots))
      call$xlim <- substitute(range(x[is.finite(x)]))
    if (!exists("xlab", dots))
      call$xlab <- deparse(substitute(x))
    if (!exists("ylim", dots))
      call$ylim <- substitute(range(y[is.finite(y)]))
    if (!exists("ylab", dots))
      call$ylab <- deparse(substitute(y))
  }

  if (lcall < length(call)) {
    call[[1]] <- as.name("plot")
    return(eval(call, envir=parent.frame()))
  }

  NextMethod()

  if (missing(y)) {
    graphics::segments(x, errors_min(x), x, errors_max(x))
  } else {
    graphics::segments(errors_min(x), y, errors_max(x), y)
    graphics::segments(x, errors_min(y), x, errors_max(y))
  }
}
# nocov end
