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
