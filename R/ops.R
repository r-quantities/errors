#' @rdname groupGeneric.errors
#'
#' @details \subsection{\code{Ops}}{
#' Boolean operators drop the uncertainty (showing a warning once) and operate on the
#' numeric values. The rest of the operators propagate the uncertainty as expected from
#' the first-order Taylor series method. Any numeric operand is automatically
#' coerced to \code{errors} (showing a warning once) with no uncertainty.}
#'
#' @examples
#' y <- set_errors(4:6, 0.2)
#' x / sqrt(y) + y * sin(x)
#'
#' # numeric values are automatically coerced to errors
#' x^2
#'
#' # boolean operators drop uncertainty
#' y > x
#'
#' @export
Ops.errors <- function(e1, e2) {
  if (.Generic %in% c("&", "|", "!", "==", "!=", "<", ">", "<=", ">=")) {
    warn_once(
      "boolean operators not defined for 'errors' objects, uncertainty dropped",
      fun = .Generic,
      type = "bool"
    )
    return(NextMethod())
  }

  if (!missing(e2)) {
    coercion <- cond2int(!inherits(e1, "errors"), !inherits(e2, "errors"))
    if (coercion) {
      warn_once(
        "non-'errors' operand automatically coerced to an 'errors' object with no uncertainty",
        fun = "Ops",
        type = "coercion"
      )
      switch(coercion, e1<-set_errors(e1), e2<-set_errors(e2))
    }
  }

  deriv <- switch(
    .Generic,
    "+" = , "-" =
      if (missing(e2)) {
        e2 <- NA
        list(do.call(.Generic, list(1)), NA)
      }
      else list(1, do.call(.Generic, list(1))),
    "*" = list(.v(e2), .v(e1)),
    "/" = , "%%" = , "%/%" = list(1 / .v(e2), -.v(e1) / .v(e2)^2),
    "^" = list(.v(e1)^(.v(e2)-1) * .v(e2), .v(e1)^.v(e2) * log(abs(.v(e1))))
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
#'   warn_once(
#'     "matrix multiplication not supported for 'errors' objects, uncertainty dropped",
#'     fun = .Generic,
#'     type = "matmult"
#'   )
#'   base::`%*%`(unclass(x), unclass(y))
#' }
