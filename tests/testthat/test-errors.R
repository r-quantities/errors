context("conversion")

test_that("errors objects are correctly created", {
  xval <- c(-1, 0, 1)
  xerr <- c(0.2, 0.1, 0.18)

  x <- xval
  errors(x) <- xerr
  expect_true(inherits(x, "errors"))
  expect_equal(as.numeric(x), xval)
  expect_equal(errors(x), xerr)
  expect_equal(attr(x, "errors"), xerr)
  expect_equal(x, set_errors(xval, xerr))
  expect_equal(x, as.errors(xval, xerr))
  expect_equal(xval-xerr, errors_min(x))
  expect_equal(xval+xerr, errors_max(x))
  expect_equal(x, as.data.frame(x)$x)

  errors(x) <- xerr/2
  expect_equal(x, set_errors(x, xerr/2))
  expect_equal(x, as.errors(x, xerr/2))
})
