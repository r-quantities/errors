#' @rdname groupGeneric.errors
#'
#' @details \subsection{\code{Ops}}{
#' Boolean operators drop the uncertainty and operate on the numeric values
#' unless the option \code{errors.compare.probabilistic} is set to \code{TRUE}.
#' In such case the comparison operators return a numeric value between 0 and 1,
#' representing the probability that the comparison is true, assuming normal
#' distribution of the errors.
#' The rest of the operators propagate the uncertainty as expected from
#' the first-order Taylor series method. Any numeric operand is automatically
#' coerced to \code{errors} with no uncertainty.}
#'
#' @examples
#' y <- set_errors(1:3 + 0.1, c(0, 0.1, 0.2))
#' x / sqrt(y) + y * sin(x)
#'
#' # numeric values are automatically coerced to errors
#' x^2
#'
#' # boolean operators drop uncertainty
#' y > x
#' # unless probabilistic comparisons are enabled
#' options(errors.compare.probabilistic = TRUE)
#' y > x
#' options(errors.compare.probabilistic = NULL)
#'
#' @export
Ops.errors <- function(e1, e2) {
  if (.Generic == "!")
    return(NextMethod())

  cmp <- .Generic %in% c("==", "!=", "<", ">", "<=", ">=") # comparison-type
  pm  <- .Generic %in% c("+", "-")                         # addition-type
  prd <- .Generic %in% c("*", "/", "%/%", "%%")            # product-type
  pw  <- .Generic %in% c("**", "^")                        # power-type

  if (!any(cmp, pm, prd, pw))
    stop(paste("operation", .Generic, "not allowed"))

  if (cmp && !getOption("errors.compare.probabilistic", default=FALSE))
    return(NextMethod())

  if (!missing(e2)) {
    if (!inherits(e1, "errors")) e1 <- set_errors(e1)
    if (!inherits(e2, "errors")) e2 <- set_errors(e2)
  }

  deriv <- switch(
    .Generic,

    # comparison-type
    "==" = return(NextMethod() & ((!.e(e1) & !.e(e2)) | .c(e1, e2) == 1)),
    "!=" = return(NextMethod() | (( .e(e1) |  .e(e2)) & .c(e1, e2) != 1)),
    "<" = , "<=" = return(zstd(e2 - e1, .Generic)),
    ">" = , ">=" = return(zstd(e1 - e2, .Generic)),

    # addition-type
    "+" = , "-" =
      if (missing(e2)) {
        e2 <- NA
        list(do.call(.Generic, list(1)), NA)
      }
      else list(1, do.call(.Generic, list(1))),

    # product-type
    "*" = list(.v(e2), .v(e1)),
    "/" = list(1 / .v(e2), -.v(e1) / .v(e2)^2),
    "%/%" = return(round(e1 / e2)),
    "%%" = return(e1 - round(e1 / e2) * e2),

    # power-type
    "**" = , "^" =
      list(.v(e1)^(.v(e2)-1) * .v(e2), .v(e1)^.v(e2) * log(abs(.v(e1))))
  )

  propagate(unclass(NextMethod()), e1, e2, deriv[[1]], deriv[[2]])
}

#' #' Matrix Multiplication
#' #'
#' #' Not implemented for \code{errors} objects. Uncertainty is dropped.
#' #' @name matmult.errors
#' #' @inheritParams base::matmult
#' #' @export
#' `%*%` = function(x, y) UseMethod("%*%")
#'
#' #' @export
#' `%*%.default` = function(x, y) base::`%*%`(x, y)
#'
#' #' @export
#' `%*%.errors` = function(x, y) {
#'   warning("matrix multiplication not supported for 'errors' objects, uncertainty dropped")
#'   base::`%*%`(unclass(x), unclass(y))
#' }
