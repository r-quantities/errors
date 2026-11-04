# Coerce to a List

S3 method for `errors` objects (see
[`as.list`](https://rdrr.io/r/base/list.html)).

## Usage

``` r
# S3 method for class 'errors'
as.list(x, ...)
```

## Arguments

- x:

  object to be coerced or tested.

- ...:

  objects, possibly named.

## Examples

``` r
x <- set_errors(1:3, 0.1)
as.list(x)
#> [[1]]
#> 1.0(1)
#> 
#> [[2]]
#> 2.0(1)
#> 
#> [[3]]
#> 3.0(1)
#> 
```
