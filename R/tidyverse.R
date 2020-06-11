type_sum.errors <- function(x) {
  not <- getOption("errors.notation")
  out <- ifelse(is.null(not) || not == "parenthesis", "(err)", paste(.pm, "err"))
  structure(out, class="type_sum_errors")
}

pillar_shaft.errors <- function(x, ...) {
  not <- getOption("errors.notation")
  sep <- ifelse(is.null(not) || not == "parenthesis", "(", " ")
  out <- sapply(strsplit(format(x), "[[:space:]|\\(]"), function(x)
    paste0(x[1], pillar::style_subtle(paste0(sep, x[-1], collapse=""))))
  pillar::new_pillar_shaft_simple(out, align = "right", min_width = 8)
}

format_type_sum.type_sum_errors <- function(x, width, ...) {
  pillar::style_subtle(x)
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

vec_proxy_equal.errors <- function(x, ...) {
  warn_once_bool("vctrs::vec_proxy_equal")
  x
}
# Currently necessary because of r-lib/vctrs/issues/1140
vec_proxy_compare.errors <- function(x, ...) {
  vec_proxy_equal.errors(x)
}


# vctrs coercion -----------------------------------------------------

# Ideally this would be implemented as a higher order type, i.e. the
# coercion hierarchy would be determined by the wrapped class rather
# than by `errors` specific methods (r-lib/vctrs#1080).

errors_ptype2 <- function(x, y, ...) {
  bare_x <- drop_errors.errors(x)
  bare_y <- drop_errors.errors(y)

  common <- vctrs::vec_ptype2(bare_x, bare_y, ...)

  set_errors(common, double())
}

vec_ptype2.errors.errors <- function(x, y, ...) {
  errors_ptype2(x, y, ...)
}

vec_ptype2.errors.integer <- function(x, y, ...) {
  errors_ptype2(x, y, ...)
}
vec_ptype2.integer.errors <- function(x, y, ...) {
  errors_ptype2(x, y, ...)
}

vec_ptype2.errors.double <- function(x, y, ...) {
  errors_ptype2(x, y, ...)
}
vec_ptype2.double.errors <- function(x, y, ...) {
  errors_ptype2(x, y, ...)
}


errors_upcast <- function(x, to, ...) {
  bare_x <- drop_errors.errors(x)
  bare_to <- drop_errors.errors(to)

  # Assumes the conversion doesn't change the scale of `x`. Is this reasonable?
  out <- vctrs::vec_cast(bare_x, bare_to, ...)

  set_errors(out, errors(x))
}
errors_downcast <- function(x, to, ...) {
  bare_x <- drop_errors.errors(x)
  vctrs::vec_cast(bare_x, to, ...)
}

vec_cast.errors.errors <- function(x, to, ...) {
  errors_upcast(x, to, ...)
}

vec_cast.errors.integer <- function(x, to, ...) {
  errors_upcast(x, to, ...)
}
vec_cast.integer.errors <- function(x, to, ...) {
  errors_downcast(x, to, ...)
}

vec_cast.errors.double <- function(x, to, ...) {
  errors_upcast(x, to, ...)
}
vec_cast.double.errors <- function(x, to, ...) {
  errors_downcast(x, to, ...)
}


#nocov start
register_all_s3_methods <- function() {
  register_s3_method("pillar::type_sum", "errors")
  register_s3_method("pillar::pillar_shaft", "errors")
  register_s3_method("pillar::format_type_sum", "type_sum_errors")

  register_s3_method("vctrs::vec_proxy", "errors")
  register_s3_method("vctrs::vec_restore", "errors")
  register_s3_method("vctrs::vec_proxy_equal", "errors")
  register_s3_method("vctrs::vec_proxy_compare", "errors")

  register_s3_method("vctrs::vec_ptype2", "errors.errors")
  register_s3_method("vctrs::vec_ptype2", "errors.integer")
  register_s3_method("vctrs::vec_ptype2", "integer.errors")
  register_s3_method("vctrs::vec_ptype2", "errors.double")
  register_s3_method("vctrs::vec_ptype2", "double.errors")

  register_s3_method("vctrs::vec_cast", "errors.errors")
  register_s3_method("vctrs::vec_cast", "errors.integer")
  register_s3_method("vctrs::vec_cast", "integer.errors")
  register_s3_method("vctrs::vec_cast", "errors.double")
  register_s3_method("vctrs::vec_cast", "double.errors")
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
