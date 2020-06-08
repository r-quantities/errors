.onLoad <- function(libname, pkgname) {
  types <- c("bool", "coercion", "matmult")
  types <- paste0("errors.warn.", types)
  options(as.list(setNames(rep.int(TRUE, length(types)), types)))
  register_all_s3_methods()
}
