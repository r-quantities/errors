# Arithmetic Mean and Median Value

S3 methods for `errors` objects.

## Usage

``` r
# S3 method for class 'errors'
mean(x, trim = 0, na.rm = FALSE, ...)

# S3 method for class 'errors'
weighted.mean(x, ..., na.rm = FALSE)

# S3 method for class 'errors'
median(x, na.rm = FALSE, ...)
```

## Arguments

- x:

  an `errors` object.

- trim:

  the fraction (0 to 0.5) of observations to be trimmed from each end of
  `x` before the mean is computed. Values of trim outside that range are
  taken as the nearest endpoint.

- na.rm:

  a logical evaluating to `TRUE` or `FALSE` indicating whether `NA`
  values should be stripped before the computation proceeds.

- ...:

  further arguments passed to of from other methods.

## Value

An `errors` object.

## Details

The `mean` and `weighted.mean` methods set the uncertainty as the
maximum of the standard deviation of the mean and the (weighted) mean of
the uncertainty.

The `median` method sets the uncertainty as `1.253 * errors(mean(x))`,
which is derived from the asymptotic variance formula of the median.
Note that this value is valid only if the sample is big enough.
