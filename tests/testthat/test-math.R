context("math")

test_that("rounding methods work properly", {
  x <- set_errors(c(-2, 0, 2), 0.1)
  expect_equal(sign(x), c(-1, 0, 1))

  xval <- 1.1:10.1
  xerr <- seq(0.005, 0.05, 0.005)
  x <- set_errors(xval, xerr)

  expect_equal(as.numeric(floor(x)), floor(as.numeric(x)))
  expect_equal(errors(floor(x)), errors(x) + abs(as.numeric(x) - floor(as.numeric(x))))
  expect_equal(as.numeric(ceiling(x)), ceiling(as.numeric(x)))
  expect_equal(errors(ceiling(x)), errors(x) + abs(as.numeric(x) - ceiling(as.numeric(x))))
  expect_equal(as.numeric(trunc(x)), trunc(as.numeric(x)))
  expect_equal(errors(trunc(x)), errors(x) + abs(as.numeric(x) - trunc(as.numeric(x))))
  expect_equal(as.numeric(round(x)), round(as.numeric(x)))
  expect_equal(errors(round(x)), errors(x) + abs(as.numeric(x) - round(as.numeric(x))))
  expect_equal(as.numeric(signif(x)), signif(as.numeric(x)))
  expect_equal(errors(signif(x)), errors(x) + abs(as.numeric(x) - signif(as.numeric(x))))
})

test_that("math methods work properly", {
  Dx <- function(expr) {
    if (identical(expr, expression(asinh(x))))
      expression(1 / sqrt(1 + x^2))
    else if (identical(expr, expression(acosh(x))))
      expression(1 / (sqrt(x - 1) * sqrt(x + 1)))
    else if (identical(expr, expression(atanh(x))))
      expression(1 / (1 - x^2))
    else
      D(expr, "x")
  }

  test_expr <- function(expr) {
    res <- eval(expr)
    err <- errors(x)
    x <- as.numeric(x)
    expect_equal(res, set_errors(res, abs(err * eval(Dx(expr)))))
  }

  xval <- 1.1:10.1/100
  xerr <- seq(0.005, 0.05, 0.005)/100
  x <- set_errors(xval, xerr)

  test_expr(expression(sqrt(x)))
  test_expr(expression(exp(x)))
  test_expr(expression(log(x)))
  expect_equal(log(x, 10), log10(x))
  expect_equal(log(x, 2), log2(x))
  expect_equal(log(1 + x), log1p(x))
  expect_equal(exp(x) - 1, expm1(x))
  test_expr(expression(sin(x)))
  test_expr(expression(cos(x)))
  test_expr(expression(tan(x)))
  test_expr(expression(asin(x)))
  test_expr(expression(acos(x)))
  test_expr(expression(atan(x)))
  expect_equal(sin(pi*x), sinpi(x))
  expect_equal(cos(pi*x), cospi(x))
  expect_equal(tan(pi*x), tanpi(x))
  test_expr(expression(sinh(x)))
  test_expr(expression(cosh(x)))
  test_expr(expression(tanh(x)))
  test_expr(expression(asinh(x)))
  x <- x*100
  test_expr(expression(acosh(x)))
  x <- x/100
  test_expr(expression(atanh(x)))
})

test_that("cumulative methods work properly", {
  xval <- 1.1:10.1
  xerr <- seq(0.005, 0.05, 0.005)
  x <- set_errors(xval, xerr)

  y <- x
  for (i in 2:length(y)) y[i] <- y[i] + y[i-1]
  expect_equal(y, cumsum(x))

  y <- x
  for (i in 2:length(y)) y[i] <- y[i] * y[i-1]
  expect_equal(y, cumprod(x))

  expect_equal(cummax(x), x)
  expect_equal(cummin(x), rep(x[1], length(x)))
  expect_equal(cummax(rev(x)), rep(x[length(x)], length(x)))
  expect_equal(cummin(rev(x)), rev(x))
})
