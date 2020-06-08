library(errors)
library(dplyr, warn.conflicts=FALSE)

unlist_quantities <- function(x) {
  stopifnot(is.list(x) || is.data.frame(x))

  unlist <- function(x) {
    if (any(class(x[[1]]) %in% c("quantities", "units", "errors")))
      do.call(c, x)
    else x
  }

  if (is.data.frame(x))
    as.data.frame(lapply(x, unlist), col.names=colnames(x))
  else unlist(x)
}

iris.q <- head(iris)
for (i in 1:4)
  errors(iris.q[,i]) <- iris.q[,i] * 0.05

iris.q %>%
  group_by(Species) %>%
  summarise_all(mean) -> res

iris.q.agg <- unlist_quantities(aggregate(. ~ Species, data = iris.q, mean, simplify=FALSE))

all.equal(res$Sepal.Length, iris.q.agg$Sepal.Length)

################################################################################

library(errors)
library(dplyr, warn.conflicts=FALSE)

iris.q <- head(iris)
for (i in 1:4)
  errors(iris.q[,i]) <- iris.q[,i] * 0.05

iris.q$another <- iris.q$Sepal.Length + iris.q$Sepal.Width
iris.q <- within(iris.q, another <- Sepal.Length + Sepal.Width)
correl(iris.q$another, iris.q$Sepal.Length)
correl(iris.q$another, iris.q$Sepal.Width)

iris.q$another[1:3] <- iris.q$Sepal.Length[1:3] * iris.q$Sepal.Width[1:3]
correl(iris.q$another, iris.q$Sepal.Length)
correl(iris.q$another, iris.q$Sepal.Width)

res <- iris.q %>%
  mutate(another = Sepal.Length + Sepal.Width)
correl(res$another, res$Sepal.Length)
correl(res$another, res$Sepal.Width)

################################################################################

res.samp <- dplyr::slice_sample(res, n=nrow(res))

attr(res.samp$Sepal.Length, "id"); attr(res$Sepal.Length, "id")
attr(res.samp$another, "id"); attr(res$another, "id")
correl(res$another, res$Sepal.Length)
correl(res.samp$another, res.samp$Sepal.Length)

################################################################################

pkgs <- c("dplyr", "vctrs", "errors")
tracer <- quote({
  call <- try(match.call(), silent=TRUE)
  .trace.ind <<- .trace.ind + 1
  if (!inherits(call, "try-error")) {
    indent <- paste0(rep(" ", .trace.ind), collapse="")
    line <- paste0(indent, as.character(call))
    cat(line, "\n")
  }
})
exit <- quote({ .trace.ind <<- .trace.ind - 1 })
for (pkg in pkgs) {
  where <- asNamespace(pkg)
  funs <- ls(where)
  funs <- funs[sapply(funs, function(x) is.function(get(x, where)))]
  trace(funs, tracer=tracer, exit=exit, print=FALSE, where=where)
  #untrace(funs, where=where)
}

.trace.ind <- 0
res <- iris.q %>%
  mutate(another = Sepal.Length + Sepal.Width)
