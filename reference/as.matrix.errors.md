# Coerce to a Matrix

S3 method for `errors` objects (see
[`as.matrix`](https://rdrr.io/r/base/matrix.html)).

## Usage

``` r
# S3 method for class 'errors'
as.matrix(x, ...)
```

## Arguments

- x:

  an R object.

- ...:

  additional arguments to be passed to or from methods.

## Examples

``` r
as.matrix(set_errors(1:3, 0.1))
#> Errors: 0.1 0.1 0.1
#>      [,1]
#> [1,]    1
#> [2,]    2
#> [3,]    3
```
