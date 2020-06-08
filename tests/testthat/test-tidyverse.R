
skip_if_not_installed("vctrs")

test_that("errors are proxied and restored", {
  x <- set_errors(1:3, 1:3)

  proxy <- vctrs::vec_proxy(x)
  expect_identical(proxy, data.frame(data = 1:3, errors = as.double(1:3)))

  out <- vctrs::vec_restore(proxy, x)
  expect_errors(out, 1:3, as.double(1:3))
})

test_that("can slice errors vectors", {
  x <- set_errors(1:3, 1:3)
  out <- vctrs::vec_slice(x, 2:3)
  expect_errors(out, 2:3, as.double(2:3))
})
