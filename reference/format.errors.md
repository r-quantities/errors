# Encode `errors`

Format an `errors` object for pretty printing.

## Usage

``` r
# S3 method for class 'errors'
format(x, digits = NULL, scientific = FALSE,
  notation = getOption("errors.notation", "parenthesis"),
  decimals = getOption("errors.decimals", FALSE), ...)
```

## Arguments

- x:

  an `errors` object.

- digits:

  how many significant digits are to be used for uncertainties. The
  default, `NULL`, uses `getOption("errors.digits", 1)`. Use
  `digits="pdg"` to choose an appropriate number of digits for each
  value according to the Particle Data Group rounding rule (see
  references).

- scientific:

  logical specifying whether the elements should be encoded in
  scientific format.

- notation:

  error notation; `"parenthesis"` and `"plus-minus"` are supported
  through the `"errors.notation"` option.

- decimals:

  logical specifying whether the uncertainty should be formatted with a
  decimal point even when the `"parenthesis"` notation is used.
  Otherwise (by default), the `"parenthesis"` notation scales the
  uncertainty to match the least significant digit of the value.

- ...:

  ignored.

## References

K. Nakamura et al. (Particle Data Group), J. Phys. G 37, 075021 (2010)

## Examples

``` r
x <- set_errors(1:3*100, 1:3*100 * 0.05)
format(x)
#> [1] "100(5)"  "200(10)" "300(20)"
format(x, digits=2)
#> [1] "100.0(50)" "200(10)"   "300(15)"  
format(x, digits=2, decimals=TRUE)
#> [1] "100.0(5.0)" "200(10)"    "300(15)"   
format(x, scientific=TRUE)
#> [1] "1.00(5)e2" "2.0(1)e2"  "3.0(2)e2" 
format(x, notation="plus-minus")
#> [1] "100 ± 5"  "200 ± 10" "300 ± 20"

x <- set_errors(c(0.827, 0.827), c(0.119, 0.367))
format(x, notation="plus-minus", digits="pdg")
#> [1] "0.83 ± 0.12" "0.8 ± 0.4"  
```
