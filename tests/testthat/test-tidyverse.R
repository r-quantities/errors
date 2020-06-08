
skip_if_not_installed("vctrs")

test_that("errors are proxied and restored", {
  x <- set_errors(1:3, 1:3)

  proxy <- vctrs::vec_proxy(x)
  expect_identical(proxy, data.frame(data = 1:3, errors = 1:3 + 0.0))

  out <- vctrs::vec_restore(proxy, x)
  expect_equal(out, x)

  # Errors are always promoted by vctrs methods
  expect_identical(attr(out, "errors"), as.double(1:3))
})

test_that("can slice errors vectors", {
  x <- set_errors(1:3, 1:3 + 0.0)
  expect_equal(vctrs::vec_slice(x, 2:3), x[2:3])
})
