.pm <- intToUtf8(177)

.onLoad <- function(libname, pkgname) {
  types <- c("bool", "coercion", "matmult")
  types <- paste0("errors.warn.", types)
  options(as.list(setNames(rep.int(TRUE, length(types)), types)))
}

warn_once <- function(message, fun, type) {
  type <- paste0("errors.warn.", type)
  if (getOption(type)) {
    options(as.list(setNames(FALSE, type)))
    warning("In '", fun, "' : ", message, call. = FALSE)
  }
}

.v <- function(x) as.numeric(x)

get_exponent <- function(x) ifelse(.v(x), floor(log10(abs(.v(x)))), 0)

propagate <- function(..., method=getOption("errors.propagation", "taylor-first-order")) {
  if (method == "taylor-first-order")
    sqrt(colSums(rbind(...)^2))
  else { # nocov start
    warning("error propagation '", method, "' not supported, falling back to 'taylor-first-order'")
    options(errors.propagation = "taylor-first-order")
    propagate(...)
  } # nocov end
}

cummatrix <- function(x, fill=0) {
  t(sapply(seq_len(length(x)), function(lag) {
    c(rep(fill, lag-1), x[1:(length(x)-lag+1)])
  }))
}

cond2int <- function(...) {
  args <- c(...)
  sum(2^(seq_along(args) - 1) * args)
}
