context("summary")

test_that("summary methods work properly", {
  xval <- 1.1:10.1
  xerr <- seq(0.005, 0.05, 0.005)
  x <- set_errors(xval, xerr)

  y <- 0
  for (i in 1:length(x)) y <- y + x[i]
  expect_equal(sum(x), y)

  y <- 1
  for (i in 1:length(x)) y <- y * x[i]
  expect_equal(prod(x), y)

  expect_equal(max(x), x[length(x)])
  expect_equal(min(x), x[1])

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
