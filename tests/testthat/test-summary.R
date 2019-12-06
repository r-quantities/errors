context("summary")

test_that("summary methods work properly", {
  xval <- 1.1:10.1
  xerr <- rep(0.05, 10)
  xval[c(2, 9)] <- c(1.102, 10.098)
  xerr[c(2, 9)] <- 0.1
  x <- set_errors(xval, xerr)

  y <- 0
  for (i in 1:length(x)) y <- y + x[i]
  expect_equal(sum(x), y)

  y <- 1
  for (i in 1:length(x)) y <- y * x[i]
  expect_equal(prod(x), y)

  expect_equal(max(x), max(xval + xerr))
  expect_equal(min(x), min(xval - xerr))

  expect_equal(range(x)[1], min(x))
  expect_equal(range(x)[2], max(x))

  expect_equal(mean(x), set_errors(mean(xval), sd(xval)/sqrt(length(x))))
  expect_equal(mean(set_errors(xval, xerr*100)), set_errors(mean(xval), mean(xerr*100)))

  expect_equal(weighted.mean(x), set_errors(weighted.mean(xval), sd(xval)/sqrt(length(x))))
  expect_equal(weighted.mean(set_errors(xval, xerr*100)), set_errors(weighted.mean(xval), weighted.mean(xerr*100)))

  expect_equal(median(x), set_errors(median(xval), 1.253 * errors(mean(x))))

  expect_equal(quantile(x), quantile(xval))
  expect_equal(summary(x), summary(xval))
})

test_that("na.rm is propagated", {
  xval <- 1.1:10.1
  xerr <- rep(0.05, 10)
  xval[c(2, 9)] <- c(1.102, 10.098)
  xerr[c(2, 9)] <- 0.1
  x <- set_errors(xval, xerr)
  x[3] <- NA

  expect_equal(max(x, na.rm=TRUE), max(xval + xerr))
  expect_equal(min(x, na.rm=TRUE), min(xval - xerr))

  expect_equal(range(x, na.rm=TRUE)[1], min(x, na.rm=TRUE))
  expect_equal(range(x, na.rm=TRUE)[2], max(x, na.rm=TRUE))
})
