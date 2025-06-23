test_that("base plots work as expected", suppressWarnings({
  skip_if_not_installed("vdiffr")
  fplot <- function(...) function() plot(...)

  cars <- as.matrix(cars)
  cars <- as.data.frame(set_errors(cars, cars * 0.05))
  vdiffr::expect_doppelganger("plot x", fplot(cars$speed))
  vdiffr::expect_doppelganger("plot xy", fplot(cars$speed, cars$dist))
  vdiffr::expect_doppelganger("plot dataframe", fplot(cars))

  iris.e <- iris
  iris.e[1:4] <- lapply(iris.e[1:4], function(x) set_errors(x, x*0.02))
  # doesn't work with devtools::test()
  # vdiffr::expect_doppelganger("plot formula", plot(
  #   Sepal.Width ~ Sepal.Length, iris.e, col=Species))
  vdiffr::expect_doppelganger("plot formula", with(
    iris.e, fplot(Sepal.Length, Sepal.Width, col=Species)))
}))

test_that("ggplot2 plots work as expected", suppressWarnings({
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("ggplot2")

  library(ggplot2)

  iris.e <- iris
  iris.e[1:4] <- lapply(iris.e[1:4], function(x) set_errors(x, x*0.02))

  p0 <- ggplot(iris.e) + aes(Sepal.Length, Sepal.Width, color=Species) +
    geom_point() + theme_bw() +
    theme(legend.position="inside", legend.position.inside=c(0.6, 0.8))
  p1 <- p0 +
    geom_errorbar(aes(ymin=errors_min(Sepal.Width), ymax=errors_max(Sepal.Width))) +
    geom_errorbar(aes(xmin=errors_min(Sepal.Length), xmax=errors_max(Sepal.Length)))
  p2 <- p0 + geom_errors()
  p3 <- p0 + geom_errors(aes(x=drop_errors(Sepal.Length)))
  p4 <- p0 + geom_errors(aes(y=drop_errors(Sepal.Width)))

  vdiffr::expect_doppelganger("ggplot2 explicit", p1)
  vdiffr::expect_doppelganger("ggplot2 automatic", p2)
  vdiffr::expect_doppelganger("ggplot2 y", p3)
  vdiffr::expect_doppelganger("ggplot2 x", p4)
}))
