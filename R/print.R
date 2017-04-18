#' @export
print.errors <- function(x, digits=getOption("digits"), ...) {
  if (is.array(x) || length(x) > 1L) {
    err <- paste(errors(x)[1:min(5, length(errors(x)))], collapse=" ")
    if (length(errors(x)) > 5L)
      err <- paste(err, "...")
    cat("errors: ", err, "\n", sep = "")
    y <- unclass(x)
    attr(y, "errors") <- NULL
    print(y)
  } else {
    cat(format(unclass(x), digits = digits), " ", errors(x), "\n", sep = "")
  }
  invisible(x)
}
