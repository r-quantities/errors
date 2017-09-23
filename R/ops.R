#' @export
Ops.errors <- function(e1, e2) {
  if (.Generic %in% c("&", "|", "!", "==", "!=", "<", ">", "<=", ">="))
    stop("boolean operators not allowed for 'errors' objects")

  if (!missing(e2)) switch(
    cond2int(!inherits(e1, "errors"), !inherits(e2, "errors")), {
      warning("first operand automatically coerced to an 'errors' object with zero error")
      e1 <- set_errors(e1)
    }, {
      warning("second operand automatically coerced to an 'errors' object with zero error")
      e2 <- set_errors(e2)
    }
  )

  e <- switch(
    .Generic,
    `+` = , `-` =
      if (!missing(e2))
        propagate(errors(e1), errors(e2))
      else errors(e1),
    `*` =
      propagate(errors(e1) * .v(e2),
                errors(e2) * .v(e1)),
    `/` = , `%%` = , `%/%` =
      propagate(errors(e1) / .v(e2),
                errors(e2) * .v(e1) / .v(e2)^2),
    `^` =
      propagate(errors(e1) * .v(e1)^(.v(e2)-1) * .v(e2),
                errors(e2) * .v(e1)^.v(e2) * log(abs(.v(e1))))
  )
  structure(NextMethod(), "errors" = e, class = "errors")
}

#' Matrix Multiplication
#' @name matmult
#' @param x numeric matrix or vector
#' @param y numeric matrix or vector
#' @export
`%*%` = function(x, y) UseMethod("%*%")

#' @export
`%*%.default` = function(x, y) base::`%*%`(x, y)

#' @export
`%*%.errors` = function(x, y) {
  warning("propagation not supported, errors dropped")
  base::`%*%`(unclass(x), unclass(y))
}
