# Coerce to a Data Frame

S3 method for `errors` objects (see
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)).

## Usage

``` r
# S3 method for class 'errors'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  any R object.

- row.names:

  `NULL` or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  logical. If `TRUE`, setting row names and converting column names (to
  syntactic names: see
  [`make.names`](https://rdrr.io/r/base/make.names.html)) is optional.
  Note that all of R's base package
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods
  use `optional` only for column names treatment, basically with the
  meaning of
  [`data.frame`](https://rdrr.io/r/base/data.frame.html)`(*, check.names = !optional)`.
  See also the `make.names` argument of the `matrix` method.

- ...:

  additional arguments to be passed to or from methods.

## Examples

``` r
x <- set_errors(1:3, 0.1)
y <- set_errors(4:6, 0.2)
(z <- cbind(x, y))
#> Errors: 0.1 0.1 0.1 0.2 0.2 ...
#>      x y
#> [1,] 1 4
#> [2,] 2 5
#> [3,] 3 6
as.data.frame(z)
#>        x      y
#> 1 1.0(1) 4.0(2)
#> 2 2.0(1) 5.0(2)
#> 3 3.0(1) 6.0(2)
```
