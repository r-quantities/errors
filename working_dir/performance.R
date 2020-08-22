# https://github.com/r-quantities/errors/issues/39

library(errors)

x = 1:500
errors(x) = 0.1
grp = as.character(x)

for(i in 1:10) {
  starttime <- Sys.time()
  tapply(x, grp, mean, simplify=FALSE)
  print(Sys.time() - starttime)
}
