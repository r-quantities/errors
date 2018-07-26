
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://avatars1.githubusercontent.com/u/32303769?s=40&v=4"> errors: Uncertainty Propagation for R Vectors

[![Build
Status](https://travis-ci.org/r-quantities/errors.svg?branch=master)](https://travis-ci.org/r-quantities/errors)
[![Coverage
Status](https://codecov.io/gh/r-quantities/errors/branch/master/graph/badge.svg)](https://codecov.io/gh/r-quantities/errors)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/errors)](https://cran.r-project.org/package=errors)
[![Downloads](https://cranlogs.r-pkg.org/badges/errors)](https://cran.r-project.org/package=errors)

The **errors** package provides support for easurement errors in R
vectors, matrices and arrays: automatic uncertainty propagation and
reporting.

## Installation

Install the release version from CRAN:

``` r
install.packages("errors")
```

The installation from GitHub requires the
[remotes](https://cran.r-project.org/package=remotes) package.

``` r
# install.packages("remotes")
remotes::install_github("r-quantities/errors")
```

## Example

``` r
library(errors)

x <- 1:10
errors(x) <- 0.1
x
#> Errors: 0.1 0.1 0.1 0.1 0.1 ...
#>  [1]  1  2  3  4  5  6  7  8  9 10

(x <- set_errors(1:10, 1:10 * 0.05))
#> Errors: 0.05 0.10 0.15 0.20 0.25 ...
#>  [1]  1  2  3  4  5  6  7  8  9 10

(df <- data.frame(x, 3*x, x^2, sin(x), cumsum(x)))
#> Warning: In 'Ops' : non-'errors' operand automatically coerced to an
#> 'errors' object with no uncertainty
#>          x  X3...x     x.2   sin.x. cumsum.x.
#> 1  1.00(5)  3.0(2)  1.0(1)  0.84(3)   1.00(5)
#> 2   2.0(1)  6.0(3)  4.0(4)  0.91(4)    3.0(1)
#> 3   3.0(2)  9.0(5)  9.0(9)   0.1(1)    6.0(2)
#> 4   4.0(2) 12.0(6)   16(2)  -0.8(1)   10.0(3)
#> 5   5.0(2) 15.0(8)   25(2) -0.96(7)   15.0(4)
#> 6   6.0(3) 18.0(9)   36(4)  -0.3(3)   21.0(5)
#> 7   7.0(4)   21(1)   49(5)   0.7(3)   28.0(6)
#> 8   8.0(4)   24(1)   64(6)  0.99(6)   36.0(7)
#> 9   9.0(4)   27(1)   81(8)   0.4(4)   45.0(8)
#> 10 10.0(5)   30(2) 100(10)  -0.5(4)     55(1)

format(df, notation="plus-minus")
#>              x     X3...x       x.2       sin.x.   cumsum.x.
#> 1  1.00 ± 0.05  3.0 ± 0.2 1.0 ± 0.1  0.84 ± 0.03 1.00 ± 0.05
#> 2    2.0 ± 0.1  6.0 ± 0.3 4.0 ± 0.4  0.91 ± 0.04   3.0 ± 0.1
#> 3    3.0 ± 0.2  9.0 ± 0.5 9.0 ± 0.9    0.1 ± 0.1   6.0 ± 0.2
#> 4    4.0 ± 0.2 12.0 ± 0.6    16 ± 2   -0.8 ± 0.1  10.0 ± 0.3
#> 5    5.0 ± 0.2 15.0 ± 0.8    25 ± 2 -0.96 ± 0.07  15.0 ± 0.4
#> 6    6.0 ± 0.3 18.0 ± 0.9    36 ± 4   -0.3 ± 0.3  21.0 ± 0.5
#> 7    7.0 ± 0.4     21 ± 1    49 ± 5    0.7 ± 0.3  28.0 ± 0.6
#> 8    8.0 ± 0.4     24 ± 1    64 ± 6  0.99 ± 0.06  36.0 ± 0.7
#> 9    9.0 ± 0.4     27 ± 1    81 ± 8    0.4 ± 0.4  45.0 ± 0.8
#> 10  10.0 ± 0.5     30 ± 2  100 ± 10   -0.5 ± 0.4      55 ± 1

sum(x)
#> 55(1)

print(mean(x), digits=3)
#> 5.500(957)
```
