#' @export
print.errors <- function(x, ...) {
  if (is.array(x) || length(x) > 1L) {
    e <- paste(format(errors(x)[1:min(5, length(errors(x)))]), collapse=" ")
    if (length(errors(x)) > 5L)
      e <- paste(e, "...")
    cat("errors: ", e, "\n", sep = "")
    y <- unclass(x)
    attr(y, "errors") <- NULL
    print(y, ...)
  } else {
    cat(format(x, ...), "\n", sep="")
  }
  invisible(x)
}

#' Encode errors
#'
#' Format errors for pretty printing.
#'
#' @param x an \code{errors} object.
#' @param digits how many significant digits are to be used for errors. The default,
#' \code{NULL}, uses \code{getOption("errors.digits", 1)}.
#' @param scientific logical specifying whether the elements should be
#' encoded in scientific format.
#' @param notation error notation; \code{"parenthesis"} and \code{"plus-minus"}
#' are supported through the \code{"errors.notation"} option.
#' @param ... ignored.
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
  prepend <- rep("", length(x))
  append <- rep("", length(x))

  e <- signif(errors(x), digits)
  exponent <- get_exponent(x)
  value_digits <- ifelse(e, digits - get_exponent(e), getOption("digits"))
  value <- ifelse(e, signif(.v(x), exponent + value_digits), .v(x))

  cond <- (scientific | (exponent > 4 | exponent < -3)) & is.finite(e)
  e[cond] <- e[cond] * 10^(-exponent[cond])
  value[cond] <- value[cond] * 10^(-exponent[cond])
  value_digits[cond] <- digits - get_exponent(e)[cond]
  value_digits[is.infinite(value_digits)] <- 0

  if (notation == "parenthesis") {
    sep <- "("
    append[] <- ")"
    e[is.finite(e)] <- e[is.finite(e)] * 10^(pmax(0, value_digits-1))
  } else {
    sep <- " +/- "
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
