# Drop Uncertainty

Drop Uncertainty

## Usage

``` r
drop_errors(x)

# S3 method for class 'data.frame'
drop_errors(x)
```

## Arguments

- x:

  an `errors` object.

## Value

the numeric without any `errors` attributes, while preserving other
attributes like dimensions or other classes.

## Note

Equivalent to `errors(x) <- NULL` or `set_errors(x, NULL)`.
