test_that("errors objects are correctly created", {
  xval <- c(-1, 0, 1)
  xerr <- c(0.2, 0.1, 0.18)

  x <- xval
  errors(x) <- xerr
  expect_true(inherits(x, "errors"))
  expect_equal(as.numeric(x), xval)
  expect_equal(errors(x), xerr)
  expect_equal(errors(xval), rep(0, 3))
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

test_that("defaults work as expected", {
  xval <- c(0, NA, NaN, Inf)
  x <- set_errors(xval)
  expect_equal(as.numeric(x), xval)
  expect_equal(errors(x), xval)
})

test_that("errors can be defined as integers", {
  x <- set_errors(1:3, 1:3)
  expect_equal(errors(x), 1:3)
})

test_that("NA values are reintepreted as NA_real_", {
  xval <- NA
  expect_equal(errors(xval), NA_real_)
  x <- set_errors(xval)
  expect_equal(as.numeric(x), NA_real_)
  expect_equal(errors(x), NA_real_)
  x <- xval
  errors(x) <- 0
  expect_equal(as.numeric(x), NA_real_)
  expect_equal(errors(x), NA_real_)
})

test_that("errors are dropped", {
  x1 <- set_errors(1:3, 0.1)
  x2 <- set_errors(x1, NULL)
  x3 <- drop_errors(x1)
  errors(x1) <- NULL
  expect_true(!inherits(x1, "errors"))
  expect_true(!inherits(x2, "errors"))
  expect_true(!inherits(x3, "errors"))
  expect_null(attr(x1, "errors"))
  expect_null(attr(x2, "errors"))
  expect_null(attr(x3, "errors"))

  ox <- x <- data.frame(x=1:4, y=1:4)
  errors(x[[1]]) <- 0.1
  expect_s3_class(x[[1]], "errors")
  expect_identical(drop_errors(x), ox)
})
