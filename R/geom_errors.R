#' Errorbars for \code{errors} objects
#'
#' Automatic errorbars for variables with uncertainty.
#'
#' @inheritParams ggplot2::geom_errorbar
#'
#' @section Aesthetics:
#' \code{geom_errors()} understands the following aesthetics
#' (required aesthetics are in bold):
#' \itemize{
#' \item \strong{\code{x} \emph{or} \code{y}}
#' \item alpha
#' \item colour
#' \item group
#' \item linetype
#' \item linewidth
#' }
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly=TRUE)) {
#'
#' iris.e <- iris
#' iris.e[1:4] <- lapply(iris.e[1:4], function(x) set_errors(x, x*0.02))
#'
#' library(ggplot2)
#'
#' ggplot(iris.e) + aes(Sepal.Length, Sepal.Width, color=Species) +
#'   geom_point() + geom_errors()
#'
#' ggplot(iris.e) + aes(Sepal.Length, Sepal.Width, color=Species) +
#'   geom_point() + geom_errors(width=0.05, height=0.05, linewidth=0.2)
#'
#' }
#' @export
geom_errors <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        na.rm = FALSE, orientation = NA,
                        show.legend = NA, inherit.aes = TRUE) {

  if (!requireNamespace("ggplot2", quietly=TRUE))
    stop("package 'ggplot2' is required for this functionality", call.=FALSE)

  layer_errors <- ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = MakeGeomErrors(),
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )

  ggplot2::ggproto(
    "LayerErrorsInstance", layer_errors,

    compute_aesthetics = function(self, data, plot) {
      data <- ggplot2::ggproto_parent(
        layer_errors, self)$compute_aesthetics(data, plot)
      if (inherits(data$x, "errors")) {
        data$xmin_errors <- errors_min(data$x)
        data$xmax_errors <- errors_max(data$x)
        data$x <- drop_errors(data$x)
      }
      if (inherits(data$y, "errors")) {
        data$ymin_errors <- errors_min(data$y)
        data$ymax_errors <- errors_max(data$y)
        data$y <- drop_errors(data$y)
      }
      data
    },

    compute_statistic = function(self, data, layout) {
      data <- ggplot2::ggproto_parent(
        layer_errors, self)$compute_statistic(data, layout)
      compute_statistic <- getOption("errors.compute_statistic")
      if (is.function(compute_statistic))
        data <- compute_statistic(data, layout)
      data
    }
  )
}

MakeGeomErrors <- function() ggplot2::ggproto(
  "GeomErrors", ggplot2::GeomErrorbar,

  default_aes = ggplot2::aes(
    colour="black", linewidth=0.5, linetype=1, width=0.5, height=0.5, alpha=NA),

  required_aes = c("x|y"),

  setup_params = function(data, params) params,

  setup_data = function(data, params) {
    data$ymin <- data$ymin_errors
    data$ymax <- data$ymax_errors
    data <- ggplot2::GeomErrorbar$setup_data(data, params)
    data$xmin_bars <- data$xmin
    data$xmax_bars <- data$xmax
    data$ymin <- data$ymax <- NULL

    data$xmin <- data$xmin_errors
    data$xmax <- data$xmax_errors
    data <- ggplot2::GeomErrorbarh$setup_data(data, params)
    data$ymin_bars <- data$ymin
    data$ymax_bars <- data$ymax
    data$xmin <- data$xmax <- NULL

    if (!is.null(data$ymin_errors)) {
      data$ymin <- pmin(data$ymin_errors, data$ymin_bars)
      data$ymax <- pmax(data$ymax_errors, data$ymax_bars)
    }
    if (!is.null(data$xmin_errors)) {
      data$xmin <- pmin(data$xmin_errors, data$xmin_bars)
      data$xmax <- pmax(data$xmax_errors, data$xmax_bars)
    }
    data
  },

  draw_panel = function(data, panel_params, coord, width=NULL, height=NULL) {
    grob <- list()
    if (!is.null(data$ymin_errors)) {
      data$ymin <- data$ymin_errors
      data$ymax <- data$ymax_errors
      data$xmin <- data$xmin_bars
      data$xmax <- data$xmax_bars
      grob <- append(grob, list(ggplot2::GeomErrorbar$draw_panel(
        data, panel_params, coord=coord, width=width)))
    }
    if (!is.null(data$xmin_errors)) {
      data$xmin <- data$xmin_errors
      data$xmax <- data$xmax_errors
      data$ymin <- data$ymin_bars
      data$ymax <- data$ymax_bars
      grob <- append(grob, list(ggplot2::GeomErrorbarh$draw_panel(
        data, panel_params, coord=coord, height=height)))
    }
    grob <- do.call(grid::grobTree, grob)
    grob$name <- grid::grobName(grob, "geom_errors")
    grob
  }
)

# registered in .onLoad()
scale_type.errors <- function(x) "continuous"
