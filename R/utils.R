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

# ensure it's numeric
.v <- function(x) as.numeric(x)
.e <- function(x) as.numeric(errors(x))

get_exponent <- function(x) ifelse(.v(x), floor(log10(abs(.v(x)))), 0)

digits_pdg <- function(x) {
  # extract 3 highest order digits
  x <- ifelse(is.finite(x), x, 0)
  x_sci <- formatC(abs(x), digits=2, format="e", decimal.mark=".")
  x_hod <- as.integer(gsub("(\\.|e.*)", "", x_sci))

  ifelse(x_hod < 355, 2, ifelse(x_hod < 950, 1, 0))
}

propagate <- function(xx, x, y, dx, dy, method=getOption("errors.propagation", "taylor-first-order")) {
  # if y not defined, use a vector of NAs
  if (length(y) == 1 && is.na(y))
    y <- set_errors(rep(NA_real_, length(x)))

  switch(
    method,
    "taylor-first-order" = {
      # propagate variance to new object
      var <- rowSums(cbind(
        .e(x)^2 * dx^2, .e(y)^2 * dy^2, 2 * as.numeric(covar(x, y)) * dx * dy
      ), na.rm = TRUE)
      var[var < 0] <- 0
      xx <- set_errors(xx, sqrt(var))

      # propagate covariances for new object
      idx <- attr(x, "id")
      idy <- attr(y, "id")
      for (id in ids(idx, idy)) {
        idxid <- if (.id(idx) != id) .covar(idx, id) else .e(x)^2
        idyid <- if (.id(idy) != id) .covar(idy, id) else .e(y)^2
        .covar(attr(xx, "id"), id) <-
          rowSums(cbind(idxid * dx, idyid * dy), na.rm = TRUE)
      }

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

dfapply <- function(X, FUN, ...) {
  attrs <- attributes(X)
  X <- lapply(X, FUN, ...)
  attributes(X) <- attrs
  X
}

df2apply <- function(X, Y, FUN, ...) {
  attrs <- attributes(X)
  X <- mapply(FUN, X, Y, ..., SIMPLIFY=FALSE)
  attributes(X) <- attrs
  X
}

.deparse <- function(dots, symarg, deparse.level) {
  deparse.level <- as.integer(deparse.level)
  if (identical(deparse.level, -1L)) deparse.level <- 0L # R Core's hack
  stopifnot(0 <= deparse.level, deparse.level <= 2)

  nm <- c( ## 0:
    function(i) NULL,
    ## 1:
    function(i) if(is.symbol(s <- symarg[[i]])) deparse(s) else NULL,
    ## 2:
    function(i) deparse(symarg[[i]])[[1L]])[[ 1L + deparse.level ]]
  Nms <- function(i) { if(!is.null(s <- names(symarg)[i]) && nzchar(s)) s else nm(i) }

  symarg <- as.list(symarg)[-1L]
  dnames <- sapply(seq_along(dots), Nms)
  if (!all(sapply(dnames, is.null)))
    names(dots) <- dnames
  dots
}
