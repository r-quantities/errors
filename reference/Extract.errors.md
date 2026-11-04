# Extract or Replace Parts of an Object

S3 operators to extract or replace parts of `errors` objects.

## Usage

``` r
# S3 method for class 'errors'
x[...]

# S3 method for class 'errors'
x[[...]]

# S3 method for class 'errors'
x[...] <- value

# S3 method for class 'errors'
x[[...]] <- value
```

## Arguments

- x:

  object from which to extract element(s) or in which to replace
  element(s).

- ...:

  additional arguments to be passed to base methods (see
  [`Extract`](https://rdrr.io/r/base/Extract.html)).

- value:

  typically an array-like R object of a similar class as `x`.

## Examples

``` r
x <- set_errors(1:3, 0.1)
y <- set_errors(4:6, 0.2)
(z <- rbind(x, y))
#> Errors: 0.1 0.2 0.1 0.2 0.1 ...
#>   [,1] [,2] [,3]
#> x    1    2    3
#> y    4    5    6
z[2, 2]
#> 5.0(2)
z[2, 2] <- -1
errors(z[[1, 2]]) <- 0.8
z[, 2]
#> Errors: 0.8 0.0
#>  x  y 
#>  2 -1 
```
