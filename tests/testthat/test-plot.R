skip_if_not_installed("vdiffr")

test_that("base plots work as expected", suppressWarnings({
  iris.e <- iris
  iris.e[1:4] <- lapply(iris.e[1:4], function(x) set_errors(x, x*0.02))

  vdiffr::expect_doppelganger(
    "plot", plot(Sepal.Width ~ Sepal.Length, iris.e, col=Species))
}))

skip_if_not_installed("ggplot2")

test_that("ggplot2 plots work as expected", suppressWarnings({
  library(ggplot2)

  iris.e <- iris
  iris.e[1:4] <- lapply(iris.e[1:4], function(x) set_errors(x, x*0.02))

  p0 <- ggplot(iris.e) + aes(Sepal.Length, Sepal.Width, color=Species) +
    geom_point() + theme_bw() + theme(legend.position=c(0.6, 0.8))

  p1 <- p0 +
    geom_errorbar(aes(ymin=errors_min(Sepal.Width), ymax=errors_max(Sepal.Width))) +
    geom_errorbarh(aes(xmin=errors_min(Sepal.Length), xmax=errors_max(Sepal.Length)))

  p2 <- p0 + geom_errors()

  vdiffr::expect_doppelganger("ggplot2 explicit", p1)
  vdiffr::expect_doppelganger("ggplot2 automatic", p2)
}))
