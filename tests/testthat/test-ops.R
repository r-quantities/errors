context("ops")

test_that("bolean ops throw a warning", {
  xval <- 1
  x <- set_errors(xval, 1)

  expect_warning(expect_equal(!x, !xval))
  expect_warning(expect_equal(x & x, xval & xval))
  expect_warning(expect_equal(x | x, xval | xval))
  expect_warning(expect_equal(x == x, xval == xval))
  expect_warning(expect_equal(x != x, xval != xval))
  expect_warning(expect_equal(x < x, xval < xval))
  expect_warning(expect_equal(x > x, xval > xval))
  expect_warning(expect_equal(x <= x, xval <= xval))
  expect_warning(expect_equal(x >= x, xval >= xval))
})

test_that("ops with numerics throw a warning", {
  x <- set_errors(1, 1)

  expect_warning(1 + x)
  expect_warning(x + 1)
})

test_that("ops work properly", {
  xval <- -4.1:5.1
  xerr <- seq(0.005, 0.05, 0.005)
  x <- set_errors(xval, xerr)

  expect_equal(+x, x)
  expect_equal(x + set_errors(1), set_errors(xval + 1, xerr))
  expect_equal(as.numeric(x + x), as.numeric(set_errors(2) * x))
  expect_true(all(errors(x + x) < errors(set_errors(2) * x)))
  expect_equal(-x, set_errors(-xval, xerr))
  expect_equal(x - set_errors(1), set_errors(xval - 1, xerr))
  expect_equal(set_errors(2) * x, set_errors(2 * xval, 2 * xerr))
  expect_equal(as.numeric(x * x), as.numeric(x^set_errors(2)))
  expect_true(all(errors(x * x) < errors(x^set_errors(2))))
  expect_equal(x / set_errors(2), set_errors(xval / 2, xerr / 2))
  expect_equal(as.numeric(x / x), rep(1, length(x)))
  expect_true(all(errors(x / x) < 1))
  expect_true(all(errors(x / x) > 0))
  expect_equal(as.numeric(x^x), xval^xval)
  expect_equal(x %/% set_errors(3), set_errors(floor(xval/3), xerr/3))
  expect_warning(expect_equal(x %*% x, xval %*% xval))
})
