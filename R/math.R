#' S3 Group Generic Functions
#'
#' \code{Math}, \code{Ops} and \code{Summary} group generic methods for
#' \code{errors} objects with support for automatic error propagation (see
#' \code{\link[base]{groupGeneric}} for a comprehensive list of available methods).
#'
#' @inheritParams base::groupGeneric
#' @name groupGeneric.errors
#'
#' @details \subsection{\code{Math}}{
#' The \code{sign} method returns a numeric value without error. \code{floor},
#' \code{ceiling}, \code{trunc}, \code{round} and \code{signif} add the rounding
#' error to the original error. \code{lgamma}, \code{gamma}, \code{digamma} and
#' \code{trigamma} are not implemented. The rest of the methods propagate the
#' error as expected from the first-order Taylor series method.}
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
    "abs" = NextMethod(),
    "sign" = as.numeric(NextMethod()),
    "sqrt" = x^set_errors(0.5),
    "floor" = , "ceiling" = , "trunc" = , "round" = , "signif" = {
      values <- .v(NextMethod())
      e <- errors(x) + abs(.v(x) - values)
      structure(values, "errors" = e, class = "errors")
    },
    {
      e <- switch(
        .Generic,
        "exp" = abs(errors(x) * exp(.v(x))),
        "log" = abs(errors(x) / .v(x) / log(if (missing(...)) exp(1) else c(...)[1])),
        "expm1" = abs(errors(x) * exp(.v(x))),
        "log1p" = abs(errors(set_errors(1)+x) / .v(set_errors(1)+x)),
        "cos" = abs(errors(x) * sin(.v(x))),
        "sin" = abs(errors(x) * cos(.v(x))),
        "tan" = errors(x) / cos(.v(x))^2,
        "cospi" = abs(errors(x) * pi * sin(pi * .v(x))),
        "sinpi" = abs(errors(x) * pi * cos(pi * .v(x))),
        "tanpi" = errors(x) * pi / cos(pi * .v(x))^2,
        "acos" = , "asin" = errors(x) / sqrt(1 - .v(x)^2),
        "atan" = errors(x) / (1 + .v(x)^2),
        "cosh" = abs(errors(x) * sinh(.v(x))),
        "sinh" = abs(errors(x) * cosh(.v(x))),
        "tanh" = errors(x) / cosh(.v(x))^2,
        "acosh" = errors(x) / sqrt(.v(x) - 1) / sqrt(.v(x) + 1),
        "asinh" = errors(x) / sqrt(1 + .v(x)^2),
        "atanh" = abs(errors(x) / (1 - .v(x)^2)),
        "cumsum" = propagate(cummatrix(errors(x)))
      )
      structure(NextMethod(), "errors" = e, class = "errors")
    },
    "cumprod" = {
      values <- NextMethod()
      e <- propagate(cummatrix(errors(x)) * t(values / t(cummatrix(.v(x), fill=1))))
      structure(values, "errors" = e, class = "errors")
    },
    "cummax" = , "cummin" = {
      values <- NextMethod()
      indexes <- which(values == .v(x))
      reps <- diff(c(indexes, length(x)+1))
      e <- rep(errors(x)[indexes], times=reps)
      structure(values, "errors" = e, class = "errors")
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
#   structure(NextMethod(), "errors" = e, class = "errors")
# }
