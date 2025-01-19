#' Extract or Replace Parts of an Object
#'
#' S3 operators to extract or replace parts of \code{errors} objects.
#'
#' @param x object from which to extract element(s) or in which to replace element(s).
#' @param ... additional arguments to be passed to base methods
#' (see \code{\link[base]{Extract}}).
#' @param value typically an array-like \R object of a similar class as \code{x}.
#' @name Extract.errors
#'
#' @examples
#' x <- set_errors(1:3, 0.1)
#' y <- set_errors(4:6, 0.2)
#' (z <- rbind(x, y))
#' z[2, 2]
#' z[2, 2] <- -1
#' errors(z[[1, 2]]) <- 0.8
#' z[, 2]
#'
#' @export
`[.errors` <- function(x, ...) {
  e <- .e(x)
  dim(e) <- dim(x)
  set_errors(unclass(NextMethod()), as.numeric(e[...]))
}

#' @rdname Extract.errors
#' @export
`[[.errors` <- function(x, ...) {
  e <- .e(x)
  dim(e) <- dim(x)
  set_errors(unclass(NextMethod()), as.numeric(e[[...]]))
}

#' @rdname Extract.errors
#' @export
`[<-.errors` <- function(x, ..., value) {
  e <- .e(x)
  dim(e) <- dim(x)
  e[...] <- .e(value)
  set_errors(unclass(NextMethod()), as.numeric(e))
}

#' @rdname Extract.errors
#' @export
`[[<-.errors` <- function(x, ..., value) {
  e <- .e(x)
  dim(e) <- dim(x)
  e[[...]] <- .e(value)
  set_errors(unclass(NextMethod()), as.numeric(e))
}

#' Replicate Elements of Vectors and Lists
#'
#' S3 method for \code{errors} objects (see \code{\link{rep}}).
#'
#' @inheritParams base::rep
#'
#' @examples
#' rep(set_errors(1, 0.1), 4)
#'
#' @export
rep.errors <- function(x, ...)
  set_errors(unclass(NextMethod()), rep(.e(x), ...))

#' Combine Values into a Vector or List
#'
#' S3 method for \code{errors} objects (see \code{\link{c}}).
#'
#' @inheritParams base::c
#'
#' @examples
#' c(set_errors(1, 0.2), set_errors(7:9, 0.1), 3)
#'
#' @export
c.errors <- function(..., recursive = FALSE)
  set_errors(unclass(NextMethod()), c(unlist(sapply(list(...), .e))))

#' Lagged Differences
#'
#' S3 method for \code{errors} objects (see \code{\link{diff}}).
#'
#' @inheritParams base::diff
#'
#' @examples
#' diff(set_errors(1:10, 0.1), 2)
#' diff(set_errors(1:10, 0.1), 2, 2)
#' x <- cumsum(cumsum(set_errors(1:10, 0.1)))
#' diff(x, lag = 2)
#' diff(x, differences = 2)
#'
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

#' Coerce to a Data Frame
#'
#' S3 method for \code{errors} objects (see \code{\link{as.data.frame}}).
#'
#' @inheritParams base::as.data.frame
#'
#' @examples
#' x <- set_errors(1:3, 0.1)
#' y <- set_errors(4:6, 0.2)
#' (z <- cbind(x, y))
#' as.data.frame(z)
#'
#' @export
as.data.frame.errors <- function(x, row.names = NULL, optional = FALSE, ...) {
  e <- .e(x)
  dim(e) <- dim(x)
  xx <- as.data.frame(unclass(x), row.names, optional, ...)
  if (!optional && ncol(xx) == 1)
    colnames(xx) <- deparse(substitute(x))
  df2apply(xx, as.data.frame(e), set_errors)
}

#' Coerce to a List
#'
#' S3 method for \code{errors} objects (see \code{\link{as.list}}).
#'
#' @inheritParams base::as.list
#'
#' @examples
#' x <- set_errors(1:3, 0.1)
#' as.list(x)
#'
#' @export
as.list.errors <- function(x, ...)
  Map(set_errors, unclass(x), .e(x))

#' Coerce to a Matrix
#'
#' S3 method for \code{errors} objects (see \code{\link{as.matrix}}).
#'
#' @inheritParams base::matrix
#'
#' @examples
#' as.matrix(set_errors(1:3, 0.1))
#'
#' @export
as.matrix.errors <- function(x, ...)
  set_errors(unclass(NextMethod()), .e(x))

#' Matrix Transpose
#'
#' S3 method for \code{errors} objects (see \code{\link{t}}).
#'
#' @inheritParams base::t
#'
#' @examples
#' a <- matrix(1:30, 5, 6)
#' errors(a) <- 1:30
#' t(a)
#'
#' @export
t.errors <- function(x) {
  e <- .e(x)
  dim(e) <- dim(x)
  set_errors(unclass(NextMethod()), as.numeric(t(e)))
}

#' Combine \R Objects by Rows or Columns
#'
#' S3 methods for \code{errors} objects (see \code{\link[base]{cbind}}).
#'
#' @inheritParams base::cbind
#' @name cbind.errors
#'
#' @seealso \code{\link{c.errors}}
#'
#' @examples
#' x <- set_errors(1, 0.1)
#' y <- set_errors(1:3, 0.2)
#' (m <- cbind(x, y)) # the '1' (= shorter vector) is recycled
#' (m <- cbind(m, 8:10)[, c(1, 3, 2)]) # insert a column
#' cbind(y, diag(3)) # vector is subset -> warning
#' cbind(0, rbind(x, y))
#'
#' @export
cbind.errors <- function(..., deparse.level = 1) {
  dots <- .deparse(list(...), substitute(list(...)), deparse.level)
  errs <- lapply(dots, function(x) {
    e <- .e(x)
    dim(e) <- dim(x)
    e
  })
  call <- as.character(match.call()[[1]])
  set_errors(
    do.call(call, c(lapply(dots, unclass), deparse.level=deparse.level)),
    as.numeric(do.call(call, c(errs, deparse.level=0)))
  )
}

#' @rdname cbind.errors
#' @export
rbind.errors <- cbind.errors

#' @export
all.equal.errors <- function(target, current, ...) {
  msg <- if (identical(attr(target, "id"), attr(current, "id")))
    "id: target and current must have different IDs, otherwise, use 'identical'"
  attr(target, "id") <- attr(current, "id") <- NULL
  res <- NextMethod()
  if (isTRUE(res) && is.null(msg))
    TRUE
  else if (!isTRUE(res))
    c(msg, res)
  else msg
}

#' @export
str.errors <- function(object, ...) {
  rval <- NULL
  file <- textConnection("rval", "w", local = TRUE)
  sink(file)
  on.exit({ sink(); close(file)})
  NextMethod(give.attr=FALSE)
  on.exit()
  sink()
  close(file)
  cat(" Errors:", sub(" 'errors' ", "", rval), "\n")
}

#' @export
duplicated.errors <- function(x, incomparables=FALSE, ...) {
  dx <- dim(x)
  dval <- if (is.null(dx))
    NextMethod() else duplicated.array(x, incomparables, ...)
  x <- .e(x)
  dim(x) <- dx
  derr <- if (is.null(dx))
    NextMethod() else duplicated.array(x, incomparables, ...)
  dval & derr
}

#' @export
anyDuplicated.errors <- function(x, incomparables=FALSE, ...) {
  if (any(dup <- duplicated(x, incomparables, ...)))
    which(dup)[1] else 0
}

#' @export
unique.errors <- function(x, incomparables=FALSE, MARGIN=1, ...) {
  dup <- duplicated(x, incomparables, ...)
  if (is.null(dim(x)))
    return(x[!dup])
  # for matrices and arrays
  args <- rep(alist(a = ), length(dim(x)))
  names(args) <- NULL
  args[[MARGIN]] <- !dup
  do.call("[", c(list(x), args, list(drop = FALSE)))
}

#' @export
na.omit.errors <- function(object, ...) {
  object[is.na(errors(object))] <- NA
  NextMethod()
}

#' @export
na.fail.errors <- na.omit.errors

#' @export
na.exclude.errors <- na.omit.errors
