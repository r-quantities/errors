context("ops")

test_that("ops work properly", {
  xval <- -4.1:5.1
  xerr <- seq(0.005, 0.05, 0.005)
  x <- set_errors(xval, xerr)

  expect_equal(+x, x)
  expect_equal(x + 1, set_errors(xval + 1, xerr))
  expect_equal(as.numeric(x + x), as.numeric(2 * x))
  expect_true(all(errors(x + x) < errors(2 * x)))
  expect_equal(-x, set_errors(-xval, xerr))
  expect_equal(x - 1, set_errors(xval - 1, xerr))
  expect_equal(2 * x, set_errors(2 * xval, 2 * xerr))
  expect_equal(as.numeric(x * x), as.numeric(x^2))
  expect_true(all(errors(x * x) < errors(x^2)))
  expect_equal(x / 2, set_errors(xval / 2, xerr / 2))
  expect_equal(as.numeric(x / x), rep(1, length(x)))
  expect_true(all(errors(x / x) < 1))
  expect_true(all(errors(x / x) > 0))
  expect_equal(as.numeric(x^x), xval^xval)
  expect_equal(x %/% 3, set_errors(floor(xval/3), xerr/3))
})
