test_that("boolean ops return probabilities", {
  xval <- 1:2
  xerr <- c(0, 1)
  x1 <- set_errors(xval, xerr)
  x2 <- set_errors(xval, xerr)
  y1 <- x1 + 1
  y2 <- set_errors(xval+1, xerr)

  old <- options(errors.compare.probabilistic = TRUE)
  on.exit(do.call(options, old), TRUE)

  # uncorrelated equal
  expect_equal(x1 <  x2, c(0, 0.5))
  expect_equal(x1 >  x2, c(0, 0.5))
  expect_equal(x1 <= x2, c(1, 0.5))
  expect_equal(x1 >= x2, c(1, 0.5))
  expect_equal(x1 == x2, c(TRUE, FALSE))
  expect_equal(x1 != x2, c(FALSE, TRUE))

  # uncorrelated different
  expect_equal(x1 <  y2, c(1, 0.7602499), tolerance=1e-6)
  expect_equal(x1 >  y2, c(0, 1 - 0.7602499), tolerance=1e-6)
  expect_equal(x1 <= y2, c(1, 0.7602499), tolerance=1e-6)
  expect_equal(x1 >= y2, c(0, 1 - 0.7602499), tolerance=1e-6)
  expect_equal(x1 == y2, c(FALSE, FALSE))
  expect_equal(x1 != y2, c(TRUE, TRUE))

  # correlated equal
  expect_equal(x1 <  x1, c(0, 0))
  expect_equal(x1 >  x1, c(0, 0))
  expect_equal(x1 <= x1, c(1, 1))
  expect_equal(x1 >= x1, c(1, 1))
  expect_equal(x1 == x1, c(TRUE, TRUE))
  expect_equal(x1 != x1, c(FALSE, FALSE))

  # correlated different
  expect_equal(x1 <  y1, c(1, 1))
  expect_equal(x1 >  y1, c(0, 0))
  expect_equal(x1 <= y1, c(1, 1))
  expect_equal(x1 >= y1, c(0, 0))
  expect_equal(x1 == y1, c(FALSE, FALSE))
  expect_equal(x1 != y1, c(TRUE, TRUE))

  # not allowed
  expect_error(!x1, "not allowed")
  expect_error(x1 & x1, "not allowed")
  expect_error(x1 | x1, "not allowed")
})

test_that("numerics are treated as numbers with no uncertainty", {
  xval <- 1:10
  xerr <- seq(0.005, 0.05, 0.005)
  x <- set_errors(xval, xerr)

  expect_equal(1 + x, set_errors(1 + xval, xerr))
  expect_equal(x + 1, set_errors(xval + 1, xerr))
})

test_that("ops work properly", {
  xval <- -4.1:5.1
  xerr <- seq(0.005, 0.05, 0.005)
  x <- set_errors(xval, xerr)
  y <- set_errors(xval, xerr)

  expect_equal(+x, x)
  expect_equal(x + set_errors(1), set_errors(xval + 1, xerr))
  expect_equal(x + x, set_errors(2) * x)
  test_expr(x + x)
  test_expr(x + y)
  expect_equal(-x, set_errors(-xval, xerr))
  expect_equal(x - set_errors(1), set_errors(xval - 1, xerr))
  expect_equal(x - x, set_errors(0) * x)
  test_expr(x - x)
  test_expr(x - y)
  expect_equal(set_errors(2) * x, set_errors(2 * xval, 2 * xerr))
  expect_equal(x * x, x^set_errors(2))
  test_expr(x * x)
  test_expr(x * y)
  expect_equal(x / set_errors(2), set_errors(xval / 2, xerr / 2))
  expect_equal(x / x, set_errors(rep(1, length(x))))
  test_expr(x / x)
  test_expr(x / y)
  expect_equal(as.numeric((x+5)^x), (xval+5)^xval)
  test_expr((x+5)^x)
  test_expr((x+5)^y)
  div <- x / set_errors(3)
  err <- abs(drop_errors(div) - round(xval/3)) + errors(div)
  expect_equal(x %/% set_errors(3), set_errors(round(xval/3), err))
  #expect_warning(expect_equal(x %*% x, xval %*% xval))
})
