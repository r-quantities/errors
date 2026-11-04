# Lagged Differences

S3 method for `errors` objects (see
[`diff`](https://rdrr.io/r/base/diff.html)).

## Usage

``` r
# S3 method for class 'errors'
diff(x, lag = 1L, differences = 1L, ...)
```

## Arguments

- x:

  a numeric vector or matrix containing the values to be differenced.

- lag:

  an integer indicating which lag to use.

- differences:

  an integer indicating the order of the difference.

- ...:

  further arguments to be passed to or from methods.

## Examples

``` r
diff(set_errors(1:10, 0.1), 2)
#> Errors: 0.1414214 0.1414214 0.1414214 0.1414214 0.1414214 ...
#> [1] 2 2 2 2 2 2 2 2
diff(set_errors(1:10, 0.1), 2, 2)
#> Errors: 0.2 0.2 0.2 0.2 0.2 ...
#> [1] 0 0 0 0 0 0
x <- cumsum(cumsum(set_errors(1:10, 0.1)))
diff(x, lag = 2)
#> Errors: 0.2645751 0.3605551 0.4582576 0.5567764 0.6557439 ...
#> [1]   9  16  25  36  49  64  81 100
diff(x, differences = 2)
#> Errors: 0.3605551 0.5000000 0.6403124 0.7810250 0.9219544 ...
#> [1]  3  4  5  6  7  8  9 10
```
