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
