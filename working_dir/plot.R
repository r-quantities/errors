library(errors)

set.seed(42)
a <- set_errors(1:10, rnorm(10, 1))
a[2] <- NA

plot(a)
plot(a, xlim=c(2, 4))
plot(a, ylim=c(2, 4))
plot(a, a)
plot(a, a, xlim=c(2, 4))
plot(a, a, ylim=c(2, 4))

ecars <- as.matrix(cars)
ecars <- as.data.frame(set_errors(ecars, ecars * 0.05))
plot(cars)
plot(ecars)
