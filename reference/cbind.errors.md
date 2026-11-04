# Combine R Objects by Rows or Columns

S3 methods for `errors` objects (see
[`cbind`](https://rdrr.io/r/base/cbind.html)).

## Usage

``` r
# S3 method for class 'errors'
cbind(..., deparse.level = 1)

# S3 method for class 'errors'
rbind(..., deparse.level = 1)
```

## Arguments

- ...:

  (generalized) vectors or matrices. These can be given as named
  arguments. Other R objects may be coerced as appropriate, or S4
  methods may be used: see sections ‘Details’ and ‘Value’. (For the
  `"data.frame"` method of `cbind` these can be further arguments to
  [`data.frame`](https://rdrr.io/r/base/data.frame.html) such as
  `stringsAsFactors`.)

- deparse.level:

  integer controlling the construction of labels in the case of
  non-matrix-like arguments (for the default method):  
  `deparse.level = 0` constructs no labels;  
  the default `deparse.level = 1` typically and `deparse.level = 2`
  always construct labels from the argument names, see the ‘Value’
  section below.

## See also

[`c.errors`](c.errors.md)

## Examples

``` r
x <- set_errors(1, 0.1)
y <- set_errors(1:3, 0.2)
(m <- cbind(x, y)) # the '1' (= shorter vector) is recycled
#> Errors: 0.1 0.1 0.1 0.2 0.2 ...
#>      x y
#> [1,] 1 1
#> [2,] 1 2
#> [3,] 1 3
(m <- cbind(m, 8:10)[, c(1, 3, 2)]) # insert a column
#> Errors: 0.1 0.1 0.1 0.0 0.0 ...
#>      x NULL y
#> [1,] 1    8 1
#> [2,] 1    9 2
#> [3,] 1   10 3
cbind(y, diag(3)) # vector is subset -> warning
#> Errors: 0.2 0.2 0.2 0.0 0.0 ...
#>      y      
#> [1,] 1 1 0 0
#> [2,] 2 0 1 0
#> [3,] 3 0 0 1
cbind(0, rbind(x, y))
#> Errors: 0.0 0.0 0.1 0.2 0.1 ...
#>   [,1] [,2] [,3] [,4]
#> x    0    1    1    1
#> y    0    1    2    3
```
