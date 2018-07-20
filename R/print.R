#' Encode \code{errors}
#'
#' Format an \code{errors} object for pretty printing.
#'
#' @param x an \code{errors} object.
#' @param digits how many significant digits are to be used for uncertainties.
#' The default, \code{NULL}, uses \code{getOption("errors.digits", 1)}.
#' @param scientific logical specifying whether the elements should be
#' encoded in scientific format.
#' @param notation error notation; \code{"parenthesis"} and \code{"plus-minus"}
#' are supported through the \code{"errors.notation"} option.
#' @param ... ignored.
#'
#' @examples
#' x <- set_errors(1:3*100, 1:3*100 * 0.05)
#' format(x)
#' format(x, digits=2)
#' format(x, scientific=TRUE)
#' format(x, notation="plus-minus")
#'
#' @export
format.errors = function(x,
                         digits = NULL,
                         scientific = FALSE,
                         notation = getOption("errors.notation", "parenthesis"),
                         ...)
{
  stopifnot(notation %in% c("parenthesis", "plus-minus"))
  if (is.null(digits))
    digits = getOption("errors.digits", 1)
  scipen <- getOption("scipen", 0)
  prepend <- rep("", length(x))
  append <- rep("", length(x))

  e <- signif(errors(x), digits)
  exponent <- get_exponent(x)
  value_digits <- ifelse(e, digits - get_exponent(e), getOption("digits"))
  value <- ifelse(e, signif(.v(x), exponent + value_digits), .v(x))

  cond <- (scientific | (exponent > 4+scipen | exponent < -3-scipen)) & is.finite(e)
  e[cond] <- e[cond] * 10^(-exponent[cond])
  value[cond] <- value[cond] * 10^(-exponent[cond])
  value_digits[cond] <- digits - get_exponent(e)[cond]
  value_digits[is.infinite(value_digits)] <- 0

  if (notation == "parenthesis") {
    sep <- "("
    append[] <- ")"
    e[is.finite(e)] <- e[is.finite(e)] * 10^(pmax(0, value_digits-1))
  } else {
    sep <- paste0(" ", .pm, " ")
    prepend[cond] <- "("
    append[cond] <- ")"
  }
  append[cond] <- paste(append[cond], "e", exponent[cond], sep="")

  value <- sapply(seq_along(value), function(i) {
    if (!is.finite(e[[i]]))
      format(.v(x)[[i]])
    else if (e[[i]])
      formatC(value[[i]], format="f", digits=max(0, value_digits[[i]]-1), decimal.mark=getOption("OutDec"))
    else format(value[[i]])
  })
  e <- formatC(e, format="fg", flag="#", digits=digits, width=digits, decimal.mark=getOption("OutDec"))
  e <- sub("\\.$", "", e)
  paste(prepend, value, sep, e, append, sep="")
}

#' Print Values
#'
#' S3 method for \code{errors} objects.
#'
#' @param x an \code{errors} object.
#' @inheritParams base::print
#'
#' @examples
#' x <- set_errors(1:10, 1:10 * 0.05)
#' print(x)
#' print(x[1:3])
#' print(x[1])
#' print(x[1], digits=2)
#' print(x[1], notation="plus-minus")
#'
#' @export
print.errors <- function(x, ...) {
  if (is.array(x) || length(x) > 1L) {
    err <- errors(x)
    e <- paste(format(err[1:min(5, length(err))]), collapse=" ")
    if (length(err) > 5L)
      e <- paste(e, "...")
    cat("Errors: ", e, "\n", sep = "")
    x <- drop_errors(x)
    NextMethod()
  } else {
    cat(format(x, ...), "\n", sep="")
    invisible(x)
  }
}
