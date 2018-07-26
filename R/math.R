#' S3 Group Generic Functions
#'
#' \code{Math}, \code{Ops} and \code{Summary} group generic methods for
#' \code{errors} objects with support for automatic uncertainty propagation (see
#' \code{\link[base]{groupGeneric}} for a comprehensive list of available methods).
#'
#' @inheritParams base::groupGeneric
#' @name groupGeneric.errors
#'
#' @details \subsection{\code{Math}}{
#' The \code{sign} method returns a numeric value without uncertainty. \code{floor},
#' \code{ceiling}, \code{trunc}, \code{round} and \code{signif} add the rounding
#' error to the original uncertainty. \code{lgamma}, \code{gamma}, \code{digamma} and
#' \code{trigamma} are not implemented. The rest of the methods propagate the
#' uncertainty as expected from the first-order Taylor series method.}
#'
#' @examples
#' x <- set_errors(1:3, 0.1)
#' exp(x)
#' log(x)
#' cumsum(x)
#' cumprod(x)
#'
#' @export
Math.errors <- function(x, ...) {
  switch(
    .Generic,
    "abs" = set_errors(unclass(NextMethod()), errors(x)),
    "sign" = drop_errors(NextMethod()),
    "sqrt" = x^set_errors(0.5),
    "floor" = , "ceiling" = , "trunc" = , "round" = , "signif" = {
      xx <- .v(NextMethod())
      set_errors(xx, errors(x) + abs(.v(x) - xx))
    },
    {
      deriv <- switch(
        .Generic,
        "exp" = , "expm1" = exp(.v(x)),
        "log" = 1 / .v(x) / log(if (missing(...)) exp(1) else c(...)[1]),
        "log1p" = 1 / .v(set_errors(1)+x),
        "cos" = -sin(.v(x)),
        "sin" = cos(.v(x)),
        "tan" = 1 / cos(.v(x))^2,
        "cospi" = -pi * sin(pi * .v(x)),
        "sinpi" = pi * cos(pi * .v(x)),
        "tanpi" = pi / cos(pi * .v(x))^2,
        "acos" = -1 / sqrt(1 - .v(x)^2),
        "asin" = 1 / sqrt(1 - .v(x)^2),
        "atan" = 1 / (1 + .v(x)^2),
        "cosh" = sinh(.v(x)),
        "sinh" = cosh(.v(x)),
        "tanh" = 1 / cosh(.v(x))^2,
        "acosh" = 1 / sqrt(.v(x) - 1) / sqrt(.v(x) + 1),
        "asinh" = 1 / sqrt(1 + .v(x)^2),
        "atanh" = 1 / (1 - .v(x)^2)
      )
      propagate(unclass(NextMethod()), x, NA, deriv, NA)
    },
    "cumsum" = set_errors(unclass(NextMethod()), sqrt(colSums(cummatrix(errors(x))^2))),
    "cumprod" = {
      xx <- NextMethod()
      e <- sqrt(colSums((cummatrix(errors(x)) * t(xx / t(cummatrix(.v(x), fill=1))))^2))
      set_errors(unclass(xx), e)
    },
    "cummax" = , "cummin" = {
      xx <- NextMethod()
      indexes <- which(xx == .v(x))
      reps <- diff(c(indexes, length(x)+1))
      e <- rep(errors(x)[indexes], times=reps)
      set_errors(unclass(xx), e)
    },
    "lgamma" = , "gamma" = , "digamma" = , "trigamma" =
      stop("method '", .Generic, "' not supported for 'errors' objects")
  )
}

#' @export
#' @method log10 errors
log10.errors <- function(x) log(x, 10)

#' @export
#' @method log2 errors
log2.errors <- function(x) log(x, 2)

# not a generic!
# atan2.errors <- function(x, y) {
#   z <- y/x
#   e <- errors(z) / (1 + .v(z)^2)
#   set_errors(unclass(NextMethod()), e)
# }
