#' Encode \code{errors}
#'
#' Format an \code{errors} object for pretty printing.
#'
#' @param x an \code{errors} object.
#' @param digits how many significant digits are to be used for uncertainties.
#' The default, \code{NULL}, uses \code{getOption("errors.digits", 1)}.
#' Use \code{digits="pdg"} to choose an appropriate number of digits for each
#' value according to the Particle Data Group rounding rule (see references).
#' @param scientific logical specifying whether the elements should be
#' encoded in scientific format.
#' @param notation error notation; \code{"parenthesis"} and \code{"plus-minus"}
#' are supported through the \code{"errors.notation"} option.
#' @param ... ignored.
#'
#' @references
#' K. Nakamura et al. (Particle Data Group), J. Phys. G 37, 075021 (2010)
#'
#' @examples
#' x <- set_errors(1:3*100, 1:3*100 * 0.05)
#' format(x)
#' format(x, digits=2)
#' format(x, scientific=TRUE)
#' format(x, notation="plus-minus")
#'
#' x <- set_errors(c(0.827, 0.827), c(0.119, 0.367))
#' format(x, notation="plus-minus", digits="pdg")
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
    digits <- getOption("errors.digits", 1)
  digits <- if (digits == "pdg") digits_pdg(.e(x)) else rep(digits, length(x))

  scipen <- getOption("scipen", 0)
  prepend <- rep("", length(x))
  append <- rep("", length(x))

  e <- signif(.e(x), digits)
  nulle <- e == 0 & !is.na(e)
  xexp <- ifelse(.v(x) == 0, get_exponent(e) + 1, get_exponent(x))
  value_digits <- ifelse(e, digits - get_exponent(e), digits)
  value <- ifelse(e, signif(.v(x), xexp + value_digits), .v(x))
  value <- ifelse(is.finite(value), value, .v(x))

  cond <- (scientific | (xexp > 4+scipen | xexp < -3-scipen)) & is.finite(e)
  e[cond] <- e[cond] * 10^(-xexp[cond])
  value[cond] <- value[cond] * 10^(-xexp[cond])
  value_digits[cond] <- digits[cond] - get_exponent(e)[cond]
  value_digits[!is.finite(value_digits)] <- 0
  value_digits[nulle] <- getOption("digits", 7)

  if (notation == "parenthesis") {
    sep <- "("
    append[] <- ")"
    e[is.finite(e)] <- (e * 10^(pmax(0, value_digits-1)))[is.finite(e)]
  } else {
    sep <- paste0(" ", .pm, " ")
    prepend[cond] <- "("
    append[cond] <- ")"
  }
  append[cond] <- paste(append[cond], "e", xexp[cond], sep="")

  value <- sapply(seq_along(value), function(i) {
    formatC(value[[i]], format="f",
            digits=max(0, value_digits[[i]]-1),
            decimal.mark=getOption("OutDec"))
  })
  value[nulle] <- prettyNum(value[nulle], drop0trailing=TRUE)

  e <- sapply(seq_along(digits), function(i) {
    formatC(e[[i]], format="fg", flag="#",
            digits=digits[[i]], width=max(1, digits[[i]]),
            decimal.mark=getOption("OutDec"))
  })
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
    err <- .e(x)
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
