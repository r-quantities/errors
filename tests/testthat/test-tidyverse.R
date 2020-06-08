
skip_if_not_installed("vctrs")

test_that("errors are proxied and restored", {
  x <- set_errors(1:3, 1:3)

  proxy <- vctrs::vec_proxy(x)
  expect_identical(proxy, data.frame(data = 1:3, errors = as.double(1:3)))

  out <- vctrs::vec_restore(proxy, x)
  expect_errors(out, 1:3, as.double(1:3))
})

test_that("can slice errors vectors", {
  x <- set_errors(1:3, 3:1)
  out <- vctrs::vec_slice(x, 2:3)
  expect_errors(out, 2:3, as.double(2:1))
})

test_that("can coerce errors vectors", {
  out <- vctrs::vec_ptype2(set_errors(1.5, 1.5), set_errors(1L, 1L))
  expect_errors(out, double(), double())

  out <- vctrs::vec_ptype2(set_errors(0L, 0L), set_errors(1L, 1L))
  expect_errors(out, integer(), double())

  out <- vctrs::vec_cast(set_errors(1:3, 1:3), set_errors(0.0, 0L))
  expect_errors(out, as.double(1:3), as.double(1:3))

  out <- vctrs::vec_cast(set_errors(as.double(1:3), 1:3), set_errors(0L, 0L))
  expect_errors(out, 1:3, as.double(1:3))
})

test_that("can coerce errors vectors with numeric vectors", {
  out <- vctrs::vec_ptype2(set_errors(1.5, 1.5), 0L)
  expect_errors(out, double(), double())

  out <- vctrs::vec_ptype2(set_errors(0L, 0L), 0L)
  expect_errors(out, integer(), double())

  out <- vctrs::vec_cast(set_errors(1:3, 1:3), 0.0)
  expect_errors(out, as.double(1:3), as.double(1:3))

  out <- vctrs::vec_cast(set_errors(as.double(1:3), 1:3), 0L)
  expect_errors(out, 1:3, as.double(1:3))
})

test_that("can combine errors vectors", {
  x <- set_errors(1:3, 3:1)

  out <- vctrs::vec_unchop(vctrs::vec_chop(x))
  expect_errors(out, 1:3, as.double(3:1))

  # Recursive case with df-cols
  x <- errors::set_errors(1:3, 3:1)
  df <- tibble::tibble(foo = tibble::tibble(x = x))
  out <- vctrs::vec_unchop(vctrs::vec_chop(df))
  expect_errors(out$foo$x, 1:3, as.double(3:1))
})

test_that("can combine errors vectors with numeric vectors", {
  x <- set_errors(1:3, 3:1)

  out <- vctrs::vec_c(x[1], 10L, x[3])
  expect_errors(out, c(1L, 10L, 3L), c(3, 0, 1))

  out <- vctrs::vec_c(x[1], 10.5, x[3])
  expect_errors(out, c(1, 10.5, 3), c(3, 0, 1))
})

test_that("can compare errors vectors", {
  x <- errors::set_errors(1:3, 3:1)

  out <- suppressWarnings(vctrs::vec_equal(x, 3:1))
  expect_identical(out, c(FALSE, TRUE, FALSE))

  out <- vctrs::vec_compare(x, 3:1)
  expect_identical(out, c(-1L, 0L, 1L))

  expect_identical(vctrs::vec_match(3, x), 3L)
  expect_errors(vctrs::vec_sort(x[3:1]), 1:3, as.double(3:1))
})
