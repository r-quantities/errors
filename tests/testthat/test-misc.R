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

test_that("errors are correctly coerced to data frame", {
  a <- 1:10
  b <- set_errors(a, a)

  expect_equal(as.data.frame(b)$b, b)
  x <- as.data.frame(cbind(a, b))
  expect_equal(x$a, set_errors(a))
  expect_equal(x$b, b)
  x <- as.data.frame(rbind(a, b))
  expect_equal(x[[1]], set_errors(c(1, 1), c(0, 1)))
  expect_equal(x[[10]], set_errors(c(10, 10), c(0, 10)))
  expect_equal(rownames(x), c("a", "b"))

  x <- data.frame(a, b)
  expect_equal(x$a, a)
  expect_equal(x$b, b)
  x <- cbind(x, a, data.frame(b))
  expect_equal(x[[3]], a)
  expect_equal(x[[4]], b)
  x <- rbind(x, a[1:4], x[1,])
  expect_equal(x[[1]], c(a, 1, 1))
  expect_equal(x[[2]], c(b, 2, set_errors(1, 1)))
  expect_equal(x[[3]], c(a, 3, 1))
  expect_equal(x[[4]], c(b, 4, set_errors(1, 1)))
})

test_that("bind methods work properly", {
  a <- 1:10
  b <- set_errors(a, a)

  x <- rbind(a, a)
  y <- rbind(b, b)
  expect_equal(as.numeric(x), as.numeric(y))
  expect_equal(rep(a, each=2), errors(y))
  expect_equal(rownames(x), c("a", "a"))
  expect_equal(rownames(y), c("b", "b"))
  x <- rbind(rbind(a, a), a)
  y <- rbind(rbind(b, b), b)
  expect_equal(as.numeric(x), as.numeric(y))
  expect_equal(rep(a, each=3), errors(y))
  expect_equal(rownames(x), c("a", "a", "a"))
  expect_equal(rownames(y), c("b", "b", "b"))
  x <- rbind(a, rbind(a, a))
  y <- rbind(b, rbind(b, b))
  expect_equal(as.numeric(x), as.numeric(y))
  expect_equal(rep(a, each=3), errors(y))
  expect_equal(rownames(x), c("a", "a", "a"))
  expect_equal(rownames(y), c("b", "b", "b"))

  x <- cbind(a, a)
  y <- cbind(b, b)
  expect_equal(as.numeric(x), as.numeric(y))
  expect_equal(rep(a, 2), errors(y))
  expect_equal(colnames(x), c("a", "a"))
  expect_equal(colnames(y), c("b", "b"))
  x <- cbind(cbind(a, a), a)
  y <- cbind(cbind(b, b), b)
  expect_equal(as.numeric(x), as.numeric(y))
  expect_equal(rep(a, 3), errors(y))
  expect_equal(colnames(x), c("a", "a", "a"))
  expect_equal(colnames(y), c("b", "b", "b"))
  x <- cbind(a, cbind(a, a))
  y <- cbind(b, cbind(b, b))
  expect_equal(as.numeric(x), as.numeric(y))
  expect_equal(rep(a, 3), errors(y))
  expect_equal(colnames(x), c("a", "a", "a"))
  expect_equal(colnames(y), c("b", "b", "b"))
})
