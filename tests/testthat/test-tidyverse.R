context("tidyverse")

test_that("pillar methods work for errors objects", {
  skip_if_not_installed("pillar")

  x <- set_errors(1, 0.1)

  expect_equal(unclass(pillar::type_sum(x)), "(err)")
  expect_s3_class(pillar::type_sum(x), "type_sum_errors")
  expect_equal(as.character(pillar::pillar_shaft(x)),
               paste0("1.0", pillar::style_subtle("(1)")))
})

skip_if_not_installed("vctrs", "0.3.1")
skip_if_not_installed("dplyr", "1.0.0")

test_that("errors are proxied and restored", {
  x <- set_errors(1:3, 1:3)

  proxy <- vctrs::vec_proxy(x)
  expect_identical(proxy, data.frame(data = 1:3, errors = as.double(1:3)))

  out <- vctrs::vec_restore(proxy, x)
  expect_equal(out, set_errors(1:3, as.double(1:3)))
})

test_that("can slice errors vectors", {
  x <- set_errors(1:3, 3:1)
  out <- vctrs::vec_slice(x, 2:3)
  expect_equal(out, set_errors(2:3, as.double(2:1)))
})

test_that("can coerce errors vectors", {
  out <- vctrs::vec_ptype2(set_errors(1.5, 1.5), set_errors(1L, 1L))
  expect_equal(out, set_errors(double(), double()))

  out <- vctrs::vec_ptype2(set_errors(0L, 0L), set_errors(1L, 1L))
  expect_equal(out, set_errors(integer(), double()))

  out <- vctrs::vec_cast(set_errors(1:3, 1:3), set_errors(0.0, 0L))
  expect_equal(out, set_errors(as.double(1:3), as.double(1:3)))

  out <- vctrs::vec_cast(set_errors(as.double(1:3), 1:3), set_errors(0L, 0L))
  expect_equal(out, set_errors(1:3, as.double(1:3)))
})

test_that("can coerce errors vectors with numeric vectors", {
  out <- vctrs::vec_ptype2(set_errors(1.5, 1.5), 0L)
  expect_equal(out, set_errors(double(), double()))

  out <- vctrs::vec_ptype2(set_errors(0L, 0L), 0L)
  expect_equal(out, set_errors(integer(), double()))

  out <- vctrs::vec_cast(set_errors(1:3, 1:3), 0.0)
  set_errors(expect_equal(out, as.double(1:3)))

  out <- vctrs::vec_cast(set_errors(as.double(1:3), 1:3), 0L)
  expect_identical(out, 1:3)
})

test_that("can combine errors vectors", {
  x <- set_errors(1:3, 3:1)

  out <- vctrs::vec_unchop(vctrs::vec_chop(x))
  expect_equal(out, set_errors(1:3, as.double(3:1)))

  # Recursive case with df-cols
  df <- dplyr::tibble(foo = dplyr::tibble(x = x))
  out <- vctrs::vec_unchop(vctrs::vec_chop(df))
  expect_equal(out$foo$x, set_errors(1:3, as.double(3:1)))
})

test_that("can combine errors vectors with numeric vectors", {
  x <- set_errors(1:3, 3:1)

  out <- vctrs::vec_c(x[1], 10L, x[3])
  expect_equal(out, set_errors(c(1L, 10L, 3L), c(3, 0, 1)))

  out <- vctrs::vec_c(x[1], 10.5, x[3])
  expect_equal(out, set_errors(c(1, 10.5, 3), c(3, 0, 1)))
})

test_that("can compare errors vectors", {
  x <- errors::set_errors(1:3, 3:1)

  out <- suppressWarnings(vctrs::vec_equal(x, 3:1))
  expect_identical(out, c(FALSE, TRUE, FALSE))

  out <- vctrs::vec_compare(x, 3:1)
  expect_identical(out, c(-1L, 0L, 1L))

  expect_identical(vctrs::vec_match(3, x), 3L)
  expect_equal(vctrs::vec_sort(x[3:1]), set_errors(1:3, as.double(3:1)))
})

`%>%` <- dplyr::`%>%`

test_that("split-apply-combine with dplyr and base agree", {
  iris2 <- iris
  for (i in 1:4)
    errors(iris2[,i]) <- iris2[,i] * 0.05

  out <- iris2 %>%
    dplyr::group_by(Species) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), mean))

  # Transform to list of lists
  out <- vctrs::vec_chop(out[2:5]) %>%
    stats::setNames(out$Species) %>%
    lapply(as.list)

  exp <- lapply(split(iris2[1:4], iris2$Species), lapply, mean)
  expect_equal(out, exp)
})

test_that("split-apply-combine with dplyr can combine integers and errors", {
  df <- dplyr::tibble(
    x = c(FALSE, TRUE, FALSE),
    y = set_errors(1:3, 3:1),
    g = 1:3
  )

  out <- df %>%
    dplyr::group_by(g) %>%
    dplyr::mutate(out = if (x) 0L else y) %>%
    dplyr::pull()

  expect_equal(out, set_errors(c(1L, 0L, 3L), c(3, 0, 1)))
})
