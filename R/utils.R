.pm <- enc2native(intToUtf8(177))

warn_once <- function(message, fun, type) {
  type <- paste0("errors.warn.", type)
  if (getOption(type)) {
    options(as.list(setNames(FALSE, type)))
    warning("In '", fun, "' : ", message, call. = FALSE)
  }
}

warn_once_bool <- function(fun) warn_once(
  "boolean operators not defined for 'errors' objects, uncertainty dropped",
  fun = fun,
  type = "bool"
)

warn_once_coercion <- function(fun) warn_once(
  "non-'errors' operand automatically coerced to an 'errors' object with no uncertainty",
  fun = "Ops",
  type = "coercion"
)

.v <- function(x) as.numeric(x)

get_exponent <- function(x) ifelse(.v(x), floor(log10(abs(.v(x)))), 0)

propagate <- function(xx, x, y, dx, dy, method=getOption("errors.propagation", "taylor-first-order")) {
  # if y not defined, use a vector of NAs
  if (length(y) == 1 && is.na(y))
    y <- set_errors(rep(NA_real_, length(x)))

  switch(
    method,
    "taylor-first-order" = {
      # propagate variance to new object
      var <- rowSums(cbind(
        errors(x)^2 * dx^2, errors(y)^2 * dy^2, 2 * covar(x, y) * dx * dy
      ), na.rm = TRUE)
      var[var < 0] <- 0
      xx <- set_errors(xx, sqrt(var))

      # propagate covariances for new object
      idx <- attr(x, "id")
      idy <- attr(y, "id")
      for (id in union(ids(idx), ids(idy))) .covar(attr(xx, "id"), id) <-
        rowSums(cbind(.covar(idx, id) * dx, .covar(idy, id) * dy), na.rm = TRUE)

      # return the object
      xx
    },
    { # nocov start
      warning("uncertainty propagation '", method, "' not supported, falling back to 'taylor-first-order'")
      options(errors.propagation = "taylor-first-order")
      propagate(xx, x, y, dx, dy)
    } # nocov end
  )
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
