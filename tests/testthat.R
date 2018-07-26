library(testthat)
library(errors)

D <- function(expr, var) {
  if (identical(expr, quote(asinh(x))))
    quote(1 / sqrt(1 + x^2))
  else if (identical(expr, quote(acosh(x))))
    quote(1 / (sqrt(x - 1) * sqrt(x + 1)))
  else if (identical(expr, quote(atanh(x))))
    quote(1 / (1 - x^2))
  else
    stats::D(expr, as.character(var))
}

test_expr <- function(expr) {
  expr <- substitute(expr)
  x <- get("x", parent.frame())
  ex <- errors(x)
  ey <- exy <- 0
  if ("y" %in% as.list(expr)) {
    y <- get("y", parent.frame())
    ey <- errors(y)
    exy <- covar(x, y)
    if (is.null(exy)) exy <- 0
  }
  object <- eval(expr)
  dx <- as.numeric(eval(D(expr, "x")))
  dy <- as.numeric(eval(D(expr, "y")))
  expected <- set_errors(
    object, sqrt(dx^2 * ex^2 + dy^2 * ey^2 + 2 * dx * dy * exy)
  )
  expect_equal(object, expected)
}

test_check("errors")
detach("package:errors", unload = TRUE)
