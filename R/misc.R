#' @export
`[.errors` <- function(x, ...) {
  e <- errors(x)
  dim(e) <- dim(x)
  structure(NextMethod(), "errors" = as.numeric(e[...]), class = "errors")
}

#' @export
`[[.errors` <- function(x, ...) {
  e <- errors(x)
  dim(e) <- dim(x)
  structure(NextMethod(), "errors" = as.numeric(e[[...]]), class = "errors")
}

#' @export
`[<-.errors` <- function(x, ..., value) {
  e <- errors(x)
  e[...] <- errors(value)
  structure(NextMethod(), "errors" = e, class = "errors")
}

#' @export
`[[<-.errors` <- function(x, ..., value) {
  e <- errors(x)
  e[[...]] <- errors(value)
  structure(NextMethod(), "errors" = e, class = "errors")
}

#' @export
rep.errors <- function(x, ...) {
  e <- rep(errors(x), ...)
  structure(NextMethod(), "errors" = e, class = "errors")
}

#' @export
c.errors <- function(..., recursive = FALSE) {
  e <- c(unlist(sapply(list(...), errors)))
  structure(NextMethod(), "errors" = e, class = "errors")
}

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

#' @export
as.data.frame.errors <- function(x, row.names = NULL, optional = FALSE, ...) {
  e <- errors(x)
  dim(e) <- dim(x)
  e <- as.data.frame(e)
  value <- as.data.frame(unclass(x), row.names, optional, ...)
  if (!optional && ncol(value) == 1)
    colnames(value) <- deparse(substitute(x))
  for (i in seq_len(ncol(value)))
    errors(value[[i]]) <- e[[i]]
  value
}

#' type_sum for tidy tibble printing
#'
#' type_sum for tidy tibble printing
#' @param x object of class errors
#' @param ... ignored
#' @export type_sum.errors
type_sum.errors <- function(x, ...) "errors"

#' @export
as.matrix.errors <- function(x, ...)
  structure(NextMethod(), "errors" = errors(x), class = "errors")

#' @export
t.errors <- function(x) {
  e <- errors(x)
  dim(e) <- dim(x)
  structure(NextMethod(), "errors" = as.numeric(t(e)), class = "errors")
}

#' @export
rbind.errors <- function(..., deparse.level = 1) {
  call <- as.character(match.call()[[1]])
  allargs <- lapply(list(...), unclass)
  names(allargs) <- sapply(substitute(list(...))[-1], deparse)
  allerrs <- lapply(list(...), function(x) {
    e <- errors(x)
    dim(e) <- dim(x)
    e
  })
  structure(
    do.call(call, c(allargs, deparse.level=deparse.level)),
    errors = as.numeric(do.call(call, allerrs)),
    class = "errors"
  )
}

#' @export
cbind.errors <- rbind.errors
