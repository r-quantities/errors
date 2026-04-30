#' Continuous scales for \code{errors} objects
#'
#' Default scales for the \code{errors} class.
#'
#' @param ... arguments passed on to the corresponding continuous scale
#' (see the manual page for each \code{scale_{type}} for details).
#'
#' @name scale_errors
#' @aliases NULL
NULL

#' @rdname scale_errors
#' @export
scale_x_errors <- function(...) {
  make_scale_errors(ggplot2::scale_x_continuous(...))
}

#' @rdname scale_errors
#' @export
scale_y_errors <- function(...) {
  make_scale_errors(ggplot2::scale_y_continuous(...))
}

#' @rdname scale_errors
#' @export
scale_colour_errors <- function(...) {
  make_scale_errors(ggplot2::scale_colour_continuous(...))
}

#' @rdname scale_errors
#' @export
scale_color_errors <- scale_colour_errors

#' @rdname scale_errors
#' @export
scale_fill_errors <- function(...) {
  make_scale_errors(ggplot2::scale_fill_continuous(...))
}

#' @rdname scale_errors
#' @export
scale_alpha_errors <- function(...) {
  make_scale_errors(ggplot2::scale_alpha(...))
}

#' @rdname scale_errors
#' @export
scale_size_errors <- function(...) {
  make_scale_errors(ggplot2::scale_size(...))
}

#' @rdname scale_errors
#' @export
scale_size_area_errors <- function(...) {
  make_scale_errors(ggplot2::scale_size_area(...))
}

#' @rdname scale_errors
#' @export
scale_radius_errors <- function(...) {
  make_scale_errors(ggplot2::scale_radius(...))
}

#' @rdname scale_errors
#' @export
scale_linewidth_errors <- function(...) {
  make_scale_errors(ggplot2::scale_linewidth(...))
}

make_scale_errors <- function(parent) {
  if (!requireNamespace("ggplot2", quietly=TRUE))
    stop("package 'ggplot2' is required for this functionality", call.=FALSE)

  ggplot2::ggproto(
    paste0(class(parent)[1], "Errors"),
    parent,

    map = function(self, x, limits = self$get_limits()) {
      # remove errors for comparisons
      ggplot2::ggproto_parent(parent, self)$map(.v(x), limits)
    }
  )
}

# registered in .onLoad()
scale_type.errors <- function(x) {
  if (!"errors" %in% .packages())
    stop("Variable of class 'errors' found, but 'errors' package is not attached.\n",
         "  Please, attach it using 'library(errors)' to properly show scales with errors.")
  c("errors", "continuous")
}
