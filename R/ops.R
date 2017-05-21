#' @export
`+.errors` <- function(e1, e2) {
  if (!missing(e2))
    err <- propagate(errors(e1), errors(e2))
  else err <- errors(e1)
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
`-.errors` <- `+.errors`

#' @export
`*.errors` <- function(e1, e2) {

  err <- propagate(errors(e1) * .v(e2),
                   errors(e2) * .v(e1))
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
`/.errors` <- function(e1, e2) {
  err <- propagate(errors(e1) / .v(e2),
                   errors(e2) * .v(e1) / .v(e2)^2)
  structure(NextMethod(), "errors" = err, class = "errors")
}

#' @export
`^.errors` <- function(e1, e2) {
  err <- propagate(errors(e1) * .v(e1)^(.v(e2)-1) * .v(e2),
                   errors(e2) * .v(e1)^.v(e2) * log(abs(.v(e1))))
  structure(NextMethod(), "errors" = err, class = "errors")
}

# `%%.errors` <- errors are the same

#' @export
`%/%.errors` <- `/.errors`

# `&.errors` <- nothing to do
# `|.errors` <- nothing to do
# `!.errors` <- nothing to do
# `==.errors` <- nothing to do
# `!=.errors` <- nothing to do
# `<.errors` <- nothing to do
# `<=.errors` <- nothing to do
# `>=.errors` <- nothing to do
# `>.errors` <- nothing to do
