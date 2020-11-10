context("correl")

test_that("wrong values fail", {
  xval <- -4.1:5.1
  xerr <- seq(0.005, 0.05, 0.005)
  x <- set_errors(xval, xerr)
  y <- set_errors(xval, xerr)

  # non-errors y
  expect_error(covar(x, 1))
  expect_error(correl(x, 1))
  expect_error(covar(x, 1) <- 0)
  expect_error(correl(x, 1) <- 0)

  # wrong lengths
  expect_error(covar(x, set_errors(1)) <- 0)
  expect_error(correl(x, set_errors(1)) <- 0)
  expect_error(covar(x, set_errors(rep(1, 2))) <- 0)
  expect_error(correl(x, set_errros(rep(1, 2))) <- 0)

  # cannot modify self-correlation (same id)
  expect_error(covar(x, x) <- 0)
  expect_error(correl(x, x) <- 0)

  # wrong range
  expect_error(covar(x, y) <- 1e6)
  expect_error(correl(x, y) <- -2)
})

test_that("covariances are correctly stored, retrieved and removed", {
  gc()
  expect_equal(length(ls(errors:::ht)), 0)

  x <- set_errors(1:10, 0.1)
  y <- set_errors(10:1, 0.2)
  z <- x

  gc()
  expect_equal(length(ls(errors:::ht)), 0)
  expect_false(identical(attr(x, "id"), attr(y, "id")))
  expect_true(identical(attr(x, "id"), attr(z, "id")))
  expect_false(.id(attr(x, "id")) %in% ls(errors:::ht))
  expect_false(.id(attr(y, "id")) %in% ls(errors:::ht))
  expect_equal(length(ids(attr(x, "id"))), 1)
  expect_true(.id(attr(x, "id")) %in% ids(attr(x, "id")))
  expect_equal(length(ids(attr(y, "id"))), 1)
  expect_true(.id(attr(y, "id")) %in% ids(attr(y, "id")))
  expect_equal(covar(x, x), errors(x)^2)
  expect_equal(covar(y, y), errors(y)^2)
  expect_null(covar(x, y))
  expect_null(correl(x, y))

  correl(x, y) <- 0.3

  gc()
  expect_equal(length(ls(errors:::ht)), 2)
  expect_equal(length(ids(attr(x, "id"))), 2)
  expect_true(.id(attr(y, "id")) %in% ids(attr(x, "id")))
  expect_equal(length(ids(attr(y, "id"))), 2)
  expect_true(.id(attr(x, "id")) %in% ids(attr(y, "id")))
  expect_equal(correl(x, y), rep(0.3, length(x)))
  expect_equal(correl(z, y), rep(0.3, length(x)))
  expect_equal(covar(x, y), 0.3 * errors(x) * errors(y))
  expect_equal(covar(z, y), 0.3 * errors(x) * errors(y))

  rm(z); gc()
  expect_equal(length(ls(errors:::ht)), 2)
  expect_equal(length(ids(attr(x, "id"))), 2)
  expect_true(.id(attr(y, "id")) %in% ids(attr(x, "id")))
  expect_equal(length(ids(attr(y, "id"))), 2)
  expect_true(.id(attr(x, "id")) %in% ids(attr(y, "id")))

  rm(y); gc()
  expect_equal(length(ls(errors:::ht)), 1)
  expect_equal(length(ids(attr(x, "id"))), 1)
  expect_true(.id(attr(x, "id")) %in% ids(attr(x, "id")))

  rm(x); gc()
  expect_equal(length(ls(errors:::ht)), 0)
})

test_that("pipe-friendly versions work as expected", {
  x <- set_errors(1:10, 0.1)
  y <- set_errors(10:1, 0.2)

  expect_identical(set_correl(x, y, 0.3), x)
  expect_equal(covar(x, y), 0.3 * errors(x) * errors(y))
  expect_identical(set_correl(y, x, 0.35), y)
  expect_equal(covar(x, y), 0.35 * errors(x) * errors(y))
  expect_identical(set_covar(x, y, 0.003), x)
  expect_equal(correl(x, y), 0.003 / errors(x) / errors(y))
  expect_identical(set_covar(y, x, 0.0035), y)
  expect_equal(correl(x, y), 0.0035 / errors(x) / errors(y))
})

test_that("GUM examples give the correct results", {
  skip_if(nzchar(Sys.getenv("NOT_CRAN")), "Can't test examples interactively")

  expect_output(example("errors-package"))

  expect_equal(as.numeric(R), 127.732, tolerance=0.001)
  expect_equal(errors(R), 0.071, tolerance=0.001)
  expect_equal(as.numeric(X), 219.847, tolerance=0.001)
  expect_equal(errors(X), 0.295, tolerance=0.001)
  expect_equal(as.numeric(Z), 254.260, tolerance=0.001)
  expect_equal(errors(Z), 0.236, tolerance=0.001)

  expect_equal(correl(R, X), -0.588, tolerance=0.001)
  expect_equal(correl(R, Z), -0.485, tolerance=0.001)
  expect_equal(correl(X, Z), 0.993, tolerance=0.001)

  expect_equal(as.numeric(y1), -0.1712, tolerance=0.0001)
  expect_equal(as.numeric(y2), 0.00218, tolerance=0.00001)
  expect_equal(as.numeric(b.30), -0.1494, tolerance=0.0001)
})

test_that("correlation checks have some tolerance", {
  x <- set_errors(6.6446573357e-27, 2.0000000000000001e-36)
  y <- set_errors(5.9719201914e-10, 1.8e-19)
  expect_silent(correl(x, y) <- 1)
})
