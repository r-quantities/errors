type_sum.errors <- function(x) {
  not <- getOption("errors.notation")
  out <- ifelse(is.null(not) || not == "parenthesis", "(err)", paste(.pm, "err"))
  paste0("[", out, "]")
}

pillar_shaft.errors <- function(x, ...) {
  out <- format(x)
  if (!requireNamespace("pillar", quietly = TRUE))
    return(out)

  not <- getOption("errors.notation")
  sep <- ifelse(is.null(not) || not == "parenthesis", "(", " ")
  out <- sapply(strsplit(out, "[[:space:]|\\(]"), function(x) {
    paste0(x[1], pillar::style_subtle(paste0(sep, x[-1], collapse="")))
  })
  pillar::new_pillar_shaft_simple(out, align = "right", min_width = 8)
}


# vctrs proxying and restoration -------------------------------------

vec_proxy.errors <- function(x, ...) {
  data <- drop_errors.errors(x)
  errors <- attr(x, "errors")

  # Simplifies coercion methods
  errors <- as.double(errors)

  # The `errors` are a vectorised attribute, which requires a data
  # frame proxy
  vctrs::new_data_frame(
    list(data = data, errors = errors),
    n = length(data)
  )
}
vec_restore.errors <- function(x, ...) {
  set_errors(x$data, x$errors)
}


# vctrs coercion -----------------------------------------------------

vec_ptype2.errors.errors <- function(x, y, ...) {
  bare_x <- drop_errors.errors(x)
  bare_y <- drop_errors.errors(y)

  common <- vctrs::vec_ptype2(bare_x, bare_y, ...)

  set_errors(common, double())
}
vec_cast.errors.errors <- function(x, to, ...) {
  bare_x <- drop_errors.errors(x)
  bare_to <- drop_errors.errors(to)

  # Assumes the conversion doesn't change the scale of `x`. Is this reasonable?
  out <- vctrs::vec_cast(bare_x, bare_to, ...)

  set_errors(out, errors(x))
}


#nocov start
register_all_s3_methods <- function() {
  register_s3_method("pillar::type_sum", "errors")
  register_s3_method("pillar::pillar_shaft", "errors")

  register_s3_method("vctrs::vec_proxy", "errors")
  register_s3_method("vctrs::vec_restore", "errors")

  register_s3_method("vctrs::vec_ptype2", "errors.errors")
  register_s3_method("vctrs::vec_cast", "errors.errors")
}

register_s3_method <- function(generic, class, fun=NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  if (is.null(fun))
    fun <- get(paste0(generic, ".", class), envir=parent.frame())
  stopifnot(is.function(fun))

  if (package %in% loadedNamespaces())
    registerS3method(generic, class, fun, envir=asNamespace(package))

  # Always register hook in case package is later unloaded & reloaded
  setHook(packageEvent(package, "onLoad"), function(...)
    registerS3method(generic, class, fun, envir=asNamespace(package)))
}
# nocov end
