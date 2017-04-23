#' @export
`[.errors` <- function(x, ...)
  structure(NextMethod(), "errors" = errors(x)[...], class = "errors")

#' @export
`[[.errors` <- function(x, ...)
  structure(NextMethod(), "errors" = errors(x)[[...]], class = "errors")

#' @export
`[<-.errors` <- function(x, ..., value) {
  errors(x)[...] <- errors(value)
  NextMethod()
}

#' @export
`[[<-.errors` <- function(x, ..., value) {
  errors(x)[[...]] <- errors(value)
  NextMethod()
}

#' @export
rep.errors <- function(x, ...)
  structure(NextMethod(), "errors" = rep(errors(x), ...), class = "errors")

#' @export
c.errors <- function(..., recursive = FALSE)
  structure(NextMethod(), "errors" = c(unlist(sapply(list(...), errors))), class = "errors")

#' @export
diff.errors <- function(x, lag = 1L, differences = 1L, ...) {
  ismat <- is.matrix(x)
  xlen <- if (ismat)
    dim(x)[1L]
  else length(x)
  if (length(lag) != 1L || length(differences) > 1L || lag < 1L || differences < 1L)
    stop("'lag' and 'differences' must be integers >= 1")
  if (lag * differences >= xlen)
    return(x[0L])
  r <- x
  i1 <- -seq_len(lag)
  if (ismat)
    for (i in seq_len(differences))
      r <- r[i1, , drop = FALSE] - r[-nrow(r):-(nrow(r) - lag + 1L), , drop = FALSE]
  else for (i in seq_len(differences))
    r <- r[i1] - r[-length(r):-(length(r) - lag + 1L)]
  r
}
