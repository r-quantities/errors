test_that("equality testing checks IDs", {
  x <- set_errors(1:10, 0.1)
  y <- set_errors(unclass(x), errors(x))
  z <- x

  expect_false(is.null(attr(x, "id")))
  expect_false(is.null(attr(y, "id")))
  expect_false(is.null(attr(z, "id")))
  expect_false(identical(attr(x, "id"), attr(y, "id")))
  expect_false(identical(attr(y, "id"), attr(z, "id")))
  expect_identical(attr(z, "id"), attr(x, "id"))

  expect_equal(x, y)
  expect_equal(y, z)
  expect_error(expect_equal(z, x))
  expect_error(expect_identical(x, y))
  expect_error(expect_identical(y, z))
  expect_identical(z, x)
})

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

test_that("errors are correctly coerced to a data frame", {
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

test_that("errors are correctly coerced to a list", {
  x <- set_errors(1:10, 10:1)
  y <- as.list(x)
  expect_is(y, "list")
  expect_true(all(sapply(seq_along(y), function(i) all.equal(y[[i]], x[i]))))
})

test_that("bind methods work properly", {
  a <- 1:10
  b <- set_errors(a, a)

  x <- rbind(x=a, y=a)
  y <- rbind(x=b, y=b)
  expect_equal(as.numeric(x), as.numeric(y))
  expect_equal(rep(a, each=2), errors(y))
  expect_equal(rownames(x), c("x", "y"))
  expect_equal(rownames(y), c("x", "y"))
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

  x <- cbind(x=a, y=a)
  y <- cbind(x=b, y=b)
  expect_equal(as.numeric(x), as.numeric(y))
  expect_equal(rep(a, 2), errors(y))
  expect_equal(colnames(x), c("x", "y"))
  expect_equal(colnames(y), c("x", "y"))
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

test_that("str method works as expected", {
  x <- set_errors(runif(5), 0.01)
  out <- utils::capture.output(str(x))

  header <- paste0(" Errors: num [1:", length(x), "] ")
  vec <- paste(format(x), collapse=" ")
  expect_equal(out, paste0(header, vec, " "))
})

test_that("duplicated-related methods work as expected", {
  x <- set_errors(1:4, rep(c(0.1, 0.2), 2))
  expect_equal(duplicated(x), duplicated(drop_errors(x)))
  expect_equal(anyDuplicated(x), anyDuplicated(drop_errors(x)))
  expect_equal(unique(x), x)

  x <- set_errors(rep(c(1, 2), 2), seq(0.1, 0.4, 0.1))
  expect_equal(duplicated(x), duplicated(errors(x)))
  expect_equal(anyDuplicated(x), anyDuplicated(errors(x)))
  expect_equal(unique(x), x)

  x <- set_errors(rep(c(1, 2), 2), rep(c(0.1, 0.2), 2))
  expect_equal(duplicated(x), duplicated(drop_errors(x)))
  expect_equal(anyDuplicated(x), anyDuplicated(drop_errors(x)))
  expect_equal(unique(x), x[1:2])

  x <- set_errors(rep(c(1, 2), 2), c(0.1, 0.2, 0.1, 0.3))
  expect_equal(duplicated(x), c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(duplicated(x, fromLast=TRUE), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(anyDuplicated(x), 3)
  expect_equal(anyDuplicated(x, fromLast=TRUE), 1)
  expect_equal(unique(x), x[c(1, 2, 4)])

  x <- set_errors(matrix(rep(c(1, 2), 2), 2, byrow=TRUE), rep(c(0.1, 0.2), each=2))
  expect_equal(duplicated(x), duplicated(drop_errors(x)))
  expect_equal(anyDuplicated(x), anyDuplicated(drop_errors(x)))
  expect_equal(unique(x), x[1, , drop=FALSE])

  x <- set_errors(matrix(rep(c(1, 2), 2), 2, byrow=TRUE), rep(c(0.1, 0.2), 2))
  expect_equal(duplicated(x), array(c(FALSE, FALSE), 2))
  expect_equal(anyDuplicated(x), 0)
  expect_equal(unique(x), x)
})

test_that("NA handling functions work as expected", {
  x <- set_errors(c(1, 2, NA), c(0.1, 0.1, 0.1))
  y <- set_errors(c(1, 2, 3), c(0.1, 0.1, NA))

  expect_error(na.fail(x))
  expect_error(na.fail(y))

  xo <- na.omit(x)
  attr(xo, "na.action") <- NULL
  yo <- na.omit(y)
  attr(yo, "na.action") <- NULL
  expect_equal(x[-3], xo)
  expect_equal(y[-3], yo)

  xe <- na.exclude(x)
  attr(xe, "na.action") <- NULL
  ye <- na.exclude(y)
  attr(ye, "na.action") <- NULL
  expect_equal(x[-3], xe)
  expect_equal(y[-3], ye)

  expect_identical(x, na.pass(x))
  expect_identical(y, na.pass(y))
})
