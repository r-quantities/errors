# abs.errors <- errors are the same

#' @export
sign.errors <- function(x) {
  x <- .v(x)
  NextMethod()
}

#' @export
sqrt.errors <- function(x) x^0.5

#' @export
floor.errors <- function(x, ...) {
  err <- errors(x)
  values <- .v(NextMethod())
  err_round <- abs(.v(x) - values)
  set_errors(values, err + err_round)
}

#' @export
ceiling.errors <- floor.errors

#' @export
trunc.errors <- floor.errors

#' @export
round.errors <- floor.errors

#' @export
signif.errors <- floor.errors

#' @export
exp.errors <- function(x) {
  err <- abs(errors(x) * exp(.v(x)))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
log.errors <- function(x, base = exp(1)) {
  err <- abs(errors(x) / .v(x) / log(base))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
#' @method log10 errors
log10.errors <- function(x) log(x, 10)

#' @export
#' @method log2 errors
log2.errors <- function(x) log(x, 2)

#' @export
log1p.errors <- function(x) {
  err <- abs(errors(1+x) / .v(1+x))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
expm1.errors <- function(x) {
  err <- abs(errors(x) * exp(.v(x)))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
sin.errors <- function(x) {
  err <- abs(errors(x) * cos(.v(x)))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
cos.errors <- function(x) {
  err <- abs(errors(x) * sin(.v(x)))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
tan.errors <- function(x) {
  err <- errors(x) / cos(.v(x))^2
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
asin.errors <- function(x) {
  err <- errors(x) / sqrt(1 - .v(x)^2)
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
acos.errors <- asin.errors

#' @export
atan.errors <- function(x) {
  err <- errors(x) / (1 + .v(x)^2)
  structure(NextMethod(), "errors" = err, class = "errors")
}

# not a generic!
# atan2.errors <- function(x, y) {
#   z <- y/x
#   err <- errors(z) / (1 + .v(z)^2)
#   structure(NextMethod(), "errors" = err, class = "errors")
# }

#' @export
#' @method sinpi errors
sinpi.errors <- function(x) {
  err <- abs(errors(x) * pi * cos(pi * .v(x)))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
#' @method cospi errors
cospi.errors <- function(x) {
  err <- abs(errors(x) * pi * sin(pi * .v(x)))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
#' @method tanpi errors
tanpi.errors <- function(x) {
  err <- errors(x) * pi / cos(pi * .v(x))^2
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
sinh.errors <- function(x) {
  err <- abs(errors(x) * cosh(.v(x)))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
cosh.errors <- function(x) {
  err <- abs(errors(x) * sinh(.v(x)))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
tanh.errors <- function(x) {
  err <- errors(x) / cosh(.v(x))^2
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
asinh.errors <- function(x) {
  err <- errors(x) / sqrt(1 + .v(x)^2)
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
acosh.errors <- function(x) {
  err <- errors(x) / sqrt(.v(x) - 1) / sqrt(.v(x) + 1)
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
atanh.errors <- function(x) {
  err <- abs(errors(x) / (1 - .v(x)^2))
  structure(NextMethod(), "errors" = err, class = "errors")
}

# TODO: beta/gamma family

#' @export
cumsum.errors <- function(x) {
  err <- propagate(cummatrix(errors(x)))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
cumprod.errors <- function(x) {
  values <- NextMethod()
  err <- propagate(cummatrix(errors(x)) * t(values / t(cummatrix(.v(x), fill=1))))
  set_errors(values, err)
}

#' @export
cummax.errors <- function(x) {
  values <- NextMethod()
  indexes <- which(values == .v(x))
  reps <- diff(c(indexes, length(x)+1))
  err <- rep(errors(x)[indexes], times=reps)
  set_errors(values, err)
}

#' @export
cummin.errors <- cummax.errors
