# Handle Uncertainty on a Numeric Vector

Set or retrieve uncertainty to/from numeric vectors.

## Usage

``` r
errors(x)

errors_max(x)

errors_min(x)

errors(x) <- value

set_errors(x, value = 0)

as.errors(x, value = 0)
```

## Arguments

- x:

  a numeric object, or object of class `errors`.

- value:

  a numeric vector of length 1 or the same length as `x`.

## Value

`errors` returns a vector of uncertainty. `errors_max` (`errors_min`)
returns the values plus (minus) the uncertainty.

## Details

`` `errors<-` `` sets the uncertainty values (and converts `x` into an
object of class `errors`). `set_errors` is a pipe-friendly version of
`` `errors<-` `` and returns an object of class `errors`. `as.errors` is
an alias for `set_errors`.

See [`correl`](correl.md) on how to handle correlations between pairs of
variables.

## See also

[`groupGeneric.errors`](groupGeneric.errors.md),
[`mean.errors`](mean.errors.md), [`Extract.errors`](Extract.errors.md),
[`c`](https://rdrr.io/r/base/c.html),
[`rep`](https://rdrr.io/r/base/rep.html),
[`cbind.errors`](cbind.errors.md), [`format.errors`](format.errors.md),
[`print.errors`](print.errors.md), [`plot.errors`](plot.errors.md),
[`as.data.frame.errors`](as.data.frame.errors.md),
[`as.matrix.errors`](as.matrix.errors.md),
[`t`](https://rdrr.io/r/base/t.html).

## Examples

``` r
x = 1:3
class(x)
#> [1] "integer"
x
#> [1] 1 2 3
errors(x) <- 0.1
class(x)
#> [1] "errors"
x
#> Errors: 0.1 0.1 0.1
#> [1] 1 2 3

(x <- set_errors(x, seq(0.1, 0.3, 0.1)))
#> Errors: 0.1 0.2 0.3
#> [1] 1 2 3
errors_max(x)
#> [1] 1.1 2.2 3.3
errors_min(x)
#> [1] 0.9 1.8 2.7
```
