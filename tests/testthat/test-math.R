context("math")

test_that("sign methods work properly", {
  xval <- c(-2, 0, 1)
  xerr <- c(.1, .2, .3)
  x <- set_errors(xval, xerr)

  expect_equal(as.numeric(abs(x)), abs(xval))
  expect_equal(errors(abs(x)), xerr)
  expect_equal(sign(x), sign(xval))
})

test_that("rounding methods work properly", {
  xval <- 1.1:10.1
  xerr <- seq(0.005, 0.05, 0.005)
  x <- set_errors(xval, xerr)

  expect_equal(as.numeric(floor(x)), floor(xval))
  expect_equal(errors(floor(x)), xerr + abs(xval - floor(xval)))
  expect_equal(as.numeric(ceiling(x)), ceiling(xval))
  expect_equal(errors(ceiling(x)), xerr + abs(xval - ceiling(xval)))
  expect_equal(as.numeric(trunc(x)), trunc(xval))
  expect_equal(errors(trunc(x)), xerr + abs(xval - trunc(xval)))
  expect_equal(as.numeric(round(x)), round(xval))
  expect_equal(errors(round(x)), xerr + abs(xval - round(xval)))
  expect_equal(as.numeric(signif(x)), signif(xval))
  expect_equal(errors(signif(x)), xerr + abs(xval - signif(xval)))
})

test_that("math methods work properly", {
  xval <- 1.1:10.1/100
  xerr <- seq(0.005, 0.05, 0.005)/100
  x <- set_errors(xval, xerr)

  test_expr(sqrt(x))
  test_expr(exp(x))
  test_expr(log(x))
  expect_equal(log(x, 10), log10(x))
  expect_equal(log(x, 2), log2(x))
  expect_equal(log(set_errors(1) + x), log1p(x))
  expect_equal(exp(x) - set_errors(1), expm1(x))
  test_expr(sin(x))
  test_expr(cos(x))
  test_expr(tan(x))
  test_expr(asin(x))
  test_expr(acos(x))
  test_expr(atan(x))
  expect_equal(sin(set_errors(pi)*x), sinpi(x))
  expect_equal(cos(set_errors(pi)*x), cospi(x))
  expect_equal(tan(set_errors(pi)*x), tanpi(x))
  test_expr(sinh(x))
  test_expr(cosh(x))
  test_expr(tanh(x))
  test_expr(asinh(x))
  x <- x * set_errors(100)
  test_expr(acosh(x))
  x <- x / set_errors(100)
  test_expr(atanh(x))
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
