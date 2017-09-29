library(errors)

x <- 1:10
errors(x) <- seq(0.1, 1, 0.1)
x

errors(x) <- rep(0.2, 10)
x
y <- set_errors(1, 0.5)
y

df <- data.frame(x=x)
df

mdat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE,
               dimnames = list(c("row1", "row2"),
                               c("C.1", "C.2", "C.3")))
errors(mdat) <- 0.1
mdat

x <- set_errors(rep(11111.22222, 8),
                c(12345678, 1234.5678, 12.345678, 1.2345678, .12345678, .012345678, .000012345678, .000000012345678))
x[1]
format(data.frame(x=x), scientific=TRUE)
print(set_errors(1.6e-19, 1.45e-21), digits=2)

################################################################################

a <- 1:10
b <- set_errors(a, a)

x <- data.frame(a, b)
cbind(x, a)
cbind(x, data.frame(b))
rbind(x, a[1:2])
rbind(x, x[1,])

rbind(a, a)
rbind(b, b)
rbind(rbind(a, a), a)
rbind(rbind(b, b), b)
rbind(a, rbind(a, a))
rbind(b, rbind(b, b))

cbind(a, a)
cbind(b, b)
cbind(cbind(a, a), a)
cbind(cbind(b, b), b)
cbind(a, cbind(a, a))
cbind(b, cbind(b, b))

library(dplyr)

iris_e <- iris %>%
  mutate_at(vars(-Species), funs(set_errors(., .*0.02)))

aggregate(. ~ Species, data = iris_e, mean, simplify=TRUE)

tapply(iris_e$Sepal.Length, iris_e$Species, mean, simplify=FALSE)

by(iris_e, iris_e$Species, function(i) {
  i$Species <- NULL
  do.call(c, lapply(i, mean))
})

iris_e %>%
  group_by(Species) %>%
  summarise_all(mean)
