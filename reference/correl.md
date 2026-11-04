# Handle Correlations Between `errors` Objects

Set or retrieve correlations or covariances between `errors` objects.
See the details section below.

## Usage

``` r
correl(x, y)

correl(x, y) <- value

set_correl(x, y, value)

covar(x, y)

covar(x, y) <- value

set_covar(x, y, value)
```

## Arguments

- x:

  an object of class `errors`.

- y:

  an object of class `errors`.

- value:

  a numeric vector of length 1 or the same length as `x`.

## Value

`correl` and `covar` return a vector of correlations and covariances
respectively (or `NULL`). `set_correl` and `set_covar`, which are
pipe-friendly versions of the setters, return the `x` object.

## Details

The uncertainties associated to `errors` objects are supposed to be
independent by default. If there is some known correlation, it can be
defined using these methods, and it will be used for the propagation of
the uncertainty by the mathematical and arithmetic operations.

The `correl` method sets or retrieves correlations, i.e., a value (or
vector of values) between `-1` and `1` (see base
[`cor`](https://rdrr.io/r/stats/cor.html) on how to compute
correlations). A covariance is just a correlation value multiplied by
the standard deviations (i.e., the standard uncertainty) of both
variables. It can be defined using the `covar` method (see base
[`cov`](https://rdrr.io/r/stats/cor.html) on how to compute
covariances). These methods are equivalent; in fact, `correl` calls
`covar` internally.

Every `errors` object has a unique ID, and pairwise correlations are
stored in an internal hash table. All the functions or methods that
modify somehow the dimensions of `errors` objects (i.e., subsets, binds,
concatenations, summaries...) generate new objects with new IDs, and
correlations are not, and cannot be, propagated. Only mathematical and
arithmetic operations propagate correlations, where appropriate,
following the Taylor series method.

## Examples

``` r
x <- set_errors(1:5, 0.1)
y <- set_errors(1:5, 0.1)

# Self-correlation is of course 1, and cannot be changed
correl(x, x)
#> [1] 1 1 1 1 1
if (FALSE) { # \dontrun{
correl(x, x) <- 0.5} # }

# Cross-correlation can be set, but must be a value between -1 and 1
correl(x, y)
#> NULL
if (FALSE) { # \dontrun{
correl(x, y) <- 1.5} # }
correl(x, y) <- runif(length(x))
correl(x, y)
#> [1] 0.080750138 0.834333037 0.600760886 0.157208442 0.007399441
covar(x, y)
#> [1] 8.075014e-04 8.343330e-03 6.007609e-03 1.572084e-03 7.399441e-05
```
