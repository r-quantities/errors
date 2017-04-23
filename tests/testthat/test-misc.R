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
