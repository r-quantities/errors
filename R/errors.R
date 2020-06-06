#' \pkg{errors}: Uncertainty Propagation for R Vectors
#'
#' Support for measurement errors in R vectors, matrices and arrays: automatic
#' uncertainty propagation and reporting.
#'
#' Every measurement has an unknown error associated. Uncertainty is the
#' acknowledgement of that error: we are aware that our representation of reality
#' may differ from reality itself. This package provides support for measurement
#' errors in \R vectors, matrices and arrays. Uncertainty metadata is associated
#' to quantity values (see \code{\link{errors}}), and this uncertainty is
#' automatically propagated when you operate with \code{errors} objects (see
#' \code{\link{groupGeneric.errors}}), or with \code{errors} and numeric objects
#' (then numeric values are automatically coerced to \code{errors} objects with
#' no uncertainty).
#'
#' Correlations between measurements are also supported. In particular, any
#' operation (e.g., \code{z <- x + y}) results in a correlation between output
#' and input variables (i.e., \code{z} is correlated to \code{x} and \code{y},
#' even if there was no correlation between \code{x} and \code{y}). And in
#' general, the user can establish correlations between any pair of variables
#' (see \code{\link{correl}}).
#'
#' This package treats uncertainty as coming from Gaussian and linear sources
#' (note that, even for non-Gaussian non-linear sources, this is a reasonable
#' assumption for averages of many measurements), and propagates them using the
#' first-order Taylor series method for propagation of uncertainty. Although the
#' above assumptions are valid in a wide range of applications in science and
#' engineering, the practitioner should evaluate whether they apply for each
#' particular case.
#'
#' @author Iñaki Ucar
#'
#' @references Iñaki Ucar, Edzer Pebesma and Arturo Azcorra (2018).
#' Measurement Errors in \R. \emph{The R Journal}, 10(2), 549-557.
#' \doi{10.32614/RJ-2018-075}
#'
#' @docType package
#' @import stats
#' @name errors-package
#'
#' @seealso \code{\link{datasets}} for a description of the datasets used in the
#' examples below.
#'
#' @examples
#' ## Simultaneous resistance and reactance measurements
#'
#' # Obtain mean values and uncertainty from measured values
#' V   <- mean(set_errors(GUM.H.2$V))
#' I   <- mean(set_errors(GUM.H.2$I))
#' phi <- mean(set_errors(GUM.H.2$phi))
#'
#' # Set correlations between variables
#' correl(V, I)   <- with(GUM.H.2, cor(V, I))
#' correl(V, phi) <- with(GUM.H.2, cor(V, phi))
#' correl(I, phi) <- with(GUM.H.2, cor(I, phi))
#'
#' # Computation of resistance, reactance and impedance values
#' (R <- (V / I) * cos(phi))
#' (X <- (V / I) * sin(phi))
#' (Z <- (V / I))
#'
#' # Correlations between derived quantities
#' correl(R, X)
#' correl(R, Z)
#' correl(X, Z)
#'
#' ## Calibration of a thermometer
#'
#' # Least-squares fit for a reference temperature of 20 degC
#' fit <- lm(bk ~ I(tk - 20), data = GUM.H.3)
#'
#' # Extract coefficients and set correlation using the covariance matrix
#' y1 <- set_errors(coef(fit)[1], sqrt(vcov(fit)[1, 1]))
#' y2 <- set_errors(coef(fit)[2], sqrt(vcov(fit)[2, 2]))
#' covar(y1, y2) <- vcov(fit)[1, 2]
#'
#' # Predicted correction for 30 degC
#' (b.30 <- y1 + y2 * set_errors(30 - 20))
#'
NULL

#' Handle Uncertainty on a Numeric Vector
#'
#' Set or retrieve uncertainty to/from numeric vectors.
#'
#' @param x a numeric object, or object of class \code{errors}.
#'
#' @return \code{errors} returns a vector of uncertainty. \code{errors_max}
#' (\code{errors_min}) returns the values plus (minus) the uncertainty.
#'
#' @details \code{`errors<-`} sets the uncertainty values (and converts \code{x}
#' into an object of class \code{errors}). \code{set_errors} is a pipe-friendly
#' version of \code{`errors<-`} and returns an object of class \code{errors}.
#' \code{as.errors} is an alias for \code{set_errors}.
#'
#' See \code{\link{correl}} on how to handle correlations between pairs of variables.
#'
#' @seealso
#' \code{\link{groupGeneric.errors}}, \code{\link{mean.errors}},
#' \code{\link{Extract.errors}}, \code{\link{c}}, \code{\link{rep}}, \code{\link{cbind.errors}},
#' \code{\link{format.errors}}, \code{\link{print.errors}}, \code{\link{plot.errors}},
#' \code{\link{as.data.frame.errors}}, \code{\link{as.matrix.errors}}, \code{\link{t}}.
#'
#' @examples
#' x = 1:3
#' class(x)
#' x
#' errors(x) <- 0.1
#' class(x)
#' x
#'
#' (x <- set_errors(x, seq(0.1, 0.3, 0.1)))
#' errors_max(x)
#' errors_min(x)
#'
#' @export
errors <- function(x) UseMethod("errors")

#' @export
errors.numeric <- function(x) rep(0, length(x))

#' @export
errors.logical <- function(x) {
  if (!all(is.na(x)))
    stop("x must be numeric, non-NA logical not supported")
  rep(NA_real_, length(x))
}

#' @export
errors.errors <- function(x) {
  attr(x, "errors")
}

#' @name errors
#' @export
errors_max <- function(x) UseMethod("errors_max")

#' @export
errors_max.numeric <- function(x) .v(x) + errors(x)

#' @export
errors_max.errors <- errors_max.numeric

#' @name errors
#' @export
errors_min <- function(x) UseMethod("errors_min")

#' @export
errors_min.numeric <- function(x) .v(x) - errors(x)

#' @export
errors_min.errors <- errors_min.numeric

#' @name errors
#' @param value a numeric vector of length 1 or the same length as \code{x}.
#' @export
`errors<-` <- function(x, value) UseMethod("errors<-")

#' @export
`errors<-.errors` <- function(x, value) {
  if(is.null(value)) return(drop_errors(x))
  stopifnot(any(length(value) == c(length(x), 1L)))

  if (length(value) == 1)
    value <- rep(value, length(x))
  value[!is.finite(x)] <- x[!is.finite(x)]
  attr(x, "errors") <- abs(value)
  .covar(attr(x, "id"), attr(x, "id")) <- value^2
  class(x) <- "errors"
  x
}

#' @export
`errors<-.numeric` <- function(x, value) {
  if(is.null(value)) return(x)
  attr(x, "id") <- new_id()
  `errors<-.errors`(x, value)
}

#' @export
`errors<-.logical` <- function(x, value) {
  if (!all(is.na(x)))
    stop("x must be numeric, non-NA logical not supported")
  x <- as.numeric(x)
  errors(x) <- value
  x
}

#' @name errors
#' @export
set_errors <- function(x, value=0) UseMethod("set_errors")

#' @export
set_errors.numeric <- function(x, value=0) {
  errors(x) <- value
  x
}

#' @export
set_errors.logical <- set_errors.numeric

#' @export
set_errors.errors <- function(x, value=0)
  set_errors.numeric(drop_errors(x), value)

#' @name errors
#' @export
as.errors <- function(x, value = 0) UseMethod("as.errors")

#' @export
as.errors.default <- function(x, value = 0) set_errors(x, value)

#' Drop Uncertainty
#'
#' @param x an \code{errors} object.
#'
#' @return the numeric without any \code{errors} attributes, while preserving other
#'   attributes like dimensions or other classes.
#'
#' @note Equivalent to \code{errors(x) <- NULL} or \code{set_errors(x, NULL)}.
#'
#' @export
drop_errors <- function(x) UseMethod("drop_errors")

#' @export
drop_errors.errors <- function(x) {
  class(x) <- setdiff(class(x), "errors")
  attr(x, "errors") <- NULL
  attr(x, "id") <- NULL
  x
}

#' @name drop_errors
#' @export
drop_errors.data.frame <- function(x) {
  for (i in seq_along(x)) {
    if (inherits(x[[i]], "errors"))
      x[[i]] <- drop_errors(x[[i]])
  }
  x
}
