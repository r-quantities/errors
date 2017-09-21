context("misc")

test_that("subsetting methods work properly", {
  xval <- 1.1:10.1
  xerr <- seq(0.005, 0.05, 0.005)
  x <- set_errors(xval, xerr)

  expect_equal(x[[3]], x[3])
  expect_equal(x[c(3, 5)], set_errors(xval[c(3, 5)], xerr[c(3, 5)]))
  x[c(3, 5)] <- x[c(5, 3)]
  x[[4]] <- x[[10]]
  expect_equal(x[3:5], set_errors(xval[c(5, 10, 3)], xerr[c(5, 10, 3)]))
})

test_that("concatenation methods work properly", {
  xval <- 1.1:10.1
  xerr <- seq(0.005, 0.05, 0.005)
  x <- set_errors(xval, xerr)

  expect_equal(c(c(x, x), x), set_errors(rep(xval, 3), rep(xerr, 3)))
  expect_equal(c(c(x, x), x), rep(x, 3))
})

test_that("diff method works properly", {
  xval <- 1.1:10.1
  xerr <- seq(0.005, 0.05, 0.005)
  x <- set_errors(xval, xerr)

  y <- x
  for (i in 1:(length(y)-1)) y[i] <- y[i+1] - y[i]
  expect_equal(diff(x), y[1:(length(x)-1)])
})

test_that("matrix methods work properly", {
  x <- set_errors(1:6, 1:6)
  xm <- as.matrix(x)

  expect_equal(dim(xm), c(6, 1))
  expect_equal(as.numeric(xm), as.numeric(x))
  expect_equal(errors(xm), errors(x))

  xt <- set_errors(c(1, 4, 2, 5, 3, 6), c(1, 4, 2, 5, 3, 6))
  xm <- set_errors(cbind(1:3, 4:6), 1:6)

  expect_equal(as.numeric(xt), as.numeric(t(xm)))
  expect_equal(errors(xt), errors(t(xm)))
})
