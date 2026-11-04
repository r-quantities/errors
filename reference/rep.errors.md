# Replicate Elements of Vectors and Lists

S3 method for `errors` objects (see
[`rep`](https://rdrr.io/r/base/rep.html)).

## Usage

``` r
# S3 method for class 'errors'
rep(x, ...)
```

## Arguments

- x:

  a vector (of any mode including a
  [`list`](https://rdrr.io/r/base/list.html)) or a factor or (for `rep`
  only) a `POSIXct` or `POSIXlt` or `Date` object; or an S4 object
  containing such an object.

- ...:

  further arguments to be passed to or from other methods. For the
  internal default method these can include:

  `times`

  :   an integer-valued vector giving the (non-negative) number of times
      to repeat each element if of length `length(x) * each`, or to
      repeat the whole vector if of length 1. Negative or `NA` values
      are an error. A `double` vector is accepted, other inputs being
      coerced to an integer or double vector.

  `length.out`

  :   non-negative integer. The desired length of the output vector.
      Other inputs will be coerced to a double vector and the first
      element taken. Ignored if `NA` or invalid.

  `each`

  :   non-negative integer. Each element of `x` is repeated `each`
      times. Other inputs will be coerced to an integer or double vector
      and the first element taken. Treated as `1` if `NA` or invalid.

## Examples

``` r
rep(set_errors(1, 0.1), 4)
#> Errors: 0.1 0.1 0.1 0.1
#> [1] 1 1 1 1
```
