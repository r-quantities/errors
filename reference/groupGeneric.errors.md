# S3 Group Generic Functions

`Math`, `Ops` and `Summary` group generic methods for `errors` objects
with support for automatic uncertainty propagation (see
[`groupGeneric`](https://rdrr.io/r/base/groupGeneric.html) for a
comprehensive list of available methods).

## Usage

``` r
# S3 method for class 'errors'
Math(x, ...)

# S3 method for class 'errors'
Ops(e1, e2)

# S3 method for class 'errors'
Summary(..., na.rm = FALSE)
```

## Arguments

- x, e1, e2:

  objects.

- ...:

  further arguments passed to methods.

- na.rm:

  logical: should missing values be removed?

## Details

### `Math`

The `sign` method returns a numeric value without uncertainty. `floor`,
`ceiling`, `trunc`, `round` and `signif` add the rounding error to the
original uncertainty. `lgamma`, `gamma`, `digamma` and `trigamma` are
not implemented. The rest of the methods propagate the uncertainty as
expected from the first-order Taylor series method.

### `Ops`

Boolean operators drop the uncertainty and operate on the numeric values
unless the option `errors.compare.probabilistic` is set to `TRUE`. In
such case the comparison operators return a numeric value between 0 and
1, representing the probability that the comparison is true, assuming
normal distribution of the errors. The rest of the operators propagate
the uncertainty as expected from the first-order Taylor series method.
Any numeric operand is automatically coerced to `errors` with no
uncertainty.

### `Summary`

The methods `all` and `any` are not supported for `errors` objects and
fail with an informative message. `min`, `max` (and `range`) return the
minimum or (and) maximum value minus/plus its uncertainty. `sum` and
`prod` propagate the uncertainty as expected from the first-order Taylor
series method.

## Examples

``` r
x <- set_errors(1:3, 0.1)
exp(x)
#> Errors: 0.2718282 0.7389056 2.0085537
#> [1]  2.718282  7.389056 20.085537
log(x)
#> Errors: 0.10000000 0.05000000 0.03333333
#> [1] 0.0000000 0.6931472 1.0986123
cumsum(x)
#> Errors: 0.1000000 0.1414214 0.1732051
#> [1] 1 3 6
cumprod(x)
#> Errors: 0.1000000 0.2236068 0.7000000
#> [1] 1 2 6

y <- set_errors(1:3 + 0.1, c(0, 0.1, 0.2))
x / sqrt(y) + y * sin(x)
#> Errors: 0.15477951 0.06091015 0.25152691
#> [1] 1.879081 3.289656 2.141358

# numeric values are automatically coerced to errors
x^2
#> Errors: 0.2 0.4 0.6
#> [1] 1 4 9

# boolean operators drop uncertainty
y > x
#> [1] TRUE TRUE TRUE
# unless probabilistic comparisons are enabled
options(errors.compare.probabilistic = TRUE)
y > x
#> [1] 0.8413447 0.7602499 0.6726396
options(errors.compare.probabilistic = NULL)

c(min(x), max(x))
#> [1] 0.9 3.1
range(x)
#> [1] 0.9 3.1
sum(y)
#> 6.3(2)
prod(y)
#> 7.2(6)
```
