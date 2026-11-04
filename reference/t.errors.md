# Matrix Transpose

S3 method for `errors` objects (see
[`t`](https://rdrr.io/r/base/t.html)).

## Usage

``` r
# S3 method for class 'errors'
t(x)
```

## Arguments

- x:

  a matrix or data frame, typically.

## Examples

``` r
a <- matrix(1:30, 5, 6)
errors(a) <- 1:30
t(a)
#> Errors:  1  6 11 16 21 ...
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    2    3    4    5
#> [2,]    6    7    8    9   10
#> [3,]   11   12   13   14   15
#> [4,]   16   17   18   19   20
#> [5,]   21   22   23   24   25
#> [6,]   26   27   28   29   30
```
