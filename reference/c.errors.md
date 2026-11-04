# Combine Values into a Vector or List

S3 method for `errors` objects (see
[`c`](https://rdrr.io/r/base/c.html)).

## Usage

``` r
# S3 method for class 'errors'
c(..., recursive = FALSE)
```

## Arguments

- ...:

  objects to be concatenated. All
  [`NULL`](https://rdrr.io/r/base/NULL.html) entries are dropped before
  method dispatch unless at the very beginning of the argument list.

- recursive:

  logical. If `recursive = TRUE`, the function recursively descends
  through lists (and pairlists) combining all their elements into a
  vector.

## Examples

``` r
c(set_errors(1, 0.2), set_errors(7:9, 0.1), 3)
#> Errors: 0.2 0.1 0.1 0.1 0.0
#> [1] 1 7 8 9 3
```
