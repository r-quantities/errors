
<!-- README.md is generated from README.Rmd. Please edit that file -->
errors: Error Propagation for R Vectors
=======================================

[![Build Status](https://travis-ci.org/Enchufa2/errors.svg?branch=master)](https://travis-ci.org/Enchufa2/errors) [![Coverage Status](https://codecov.io/gh/Enchufa2/errors/branch/master/graph/badge.svg)](https://codecov.io/gh/Enchufa2/errors) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/errors)](https://cran.r-project.org/package=errors) [![Downloads](http://cranlogs.r-pkg.org/badges/errors)](https://cran.r-project.org/package=errors)

The **errors** package provides support for painless automatic error propagation in numerical operations and pretty printing.

Installation
------------

Install the release version from CRAN:

``` r
install.packages("errors")
```

The installation from GitHub requires the [devtools](https://github.com/hadley/devtools) package.

``` r
# install.packages("devtools")
devtools::install_github("Enchufa2/errors")
```

Example
-------

``` r
library(errors)

x <- 1:10
errors(x) <- 0.1
x
#> errors: 0.1 0.1 0.1 0.1 0.1 ...
#>  [1]  1  2  3  4  5  6  7  8  9 10

(x <- set_errors(1:10, seq(0.1, 1, 0.1)))
#> errors: 0.1 0.2 0.3 0.4 0.5 ...
#>  [1]  1  2  3  4  5  6  7  8  9 10

df <- as.data.frame(x)

(df$`3x` <- 3*x)
#> errors: 0.3 0.6 0.9 1.2 1.5 ...
#>  [1]  3  6  9 12 15 18 21 24 27 30

(df$`x^2` <- x^2)
#> errors: 0.2 0.8 1.8 3.2 5.0 ...
#>  [1]   1   4   9  16  25  36  49  64  81 100

(df$`sin(x)` <- sin(x))
#> errors: 0.05403023 0.08322937 0.29699775 0.26145745 0.14183109 ...
#>  [1]  0.8414710  0.9092974  0.1411200 -0.7568025 -0.9589243 -0.2794155
#>  [7]  0.6569866  0.9893582  0.4121185 -0.5440211

(df$`cumsum(x)` <- cumsum(x))
#> errors: 0.1000000 0.2236068 0.3741657 0.5477226 0.7416198 ...
#>  [1]  1  3  6 10 15 21 28 36 45 55

df
#>         x     3x     x^2  sin(x) cumsum(x)
#> 1  1.0(1) 3.0(3)  1.0(2) 0.84(5)    1.0(1)
#> 2  2.0(2) 6.0(6)  4.0(8) 0.91(8)    3.0(2)
#> 3  3.0(3) 9.0(9)    9(2)  0.1(3)    6.0(4)
#> 4  4.0(4)  12(1)   16(3) -0.8(3)   10.0(5)
#> 5  5.0(5)  15(2)   25(5) -1.0(1)   15.0(7)
#> 6  6.0(6)  18(2)   36(7) -0.3(6)     21(1)
#> 7  7.0(7)  21(2)  50(10)  0.7(5)     28(1)
#> 8  8.0(8)  24(2)  60(10)  1.0(1)     36(1)
#> 9  9.0(9)  27(3)  80(20)  0.4(8)     45(2)
#> 10  10(1)  30(3) 100(20) -0.5(8)     55(2)

options(errors.notation = "plus-minus")

df
#>              x          3x         x^2        sin(x)    cumsum(x)
#> 1  1.0 +/- 0.1 3.0 +/- 0.3 1.0 +/- 0.2 0.84 +/- 0.05  1.0 +/- 0.1
#> 2  2.0 +/- 0.2 6.0 +/- 0.6 4.0 +/- 0.8 0.91 +/- 0.08  3.0 +/- 0.2
#> 3  3.0 +/- 0.3 9.0 +/- 0.9     9 +/- 2   0.1 +/- 0.3  6.0 +/- 0.4
#> 4  4.0 +/- 0.4    12 +/- 1    16 +/- 3  -0.8 +/- 0.3 10.0 +/- 0.5
#> 5  5.0 +/- 0.5    15 +/- 2    25 +/- 5  -1.0 +/- 0.1 15.0 +/- 0.7
#> 6  6.0 +/- 0.6    18 +/- 2    36 +/- 7  -0.3 +/- 0.6     21 +/- 1
#> 7  7.0 +/- 0.7    21 +/- 2   50 +/- 10   0.7 +/- 0.5     28 +/- 1
#> 8  8.0 +/- 0.8    24 +/- 2   60 +/- 10   1.0 +/- 0.1     36 +/- 1
#> 9  9.0 +/- 0.9    27 +/- 3   80 +/- 20   0.4 +/- 0.8     45 +/- 2
#> 10    10 +/- 1    30 +/- 3  100 +/- 20  -0.5 +/- 0.8     55 +/- 2

sum(x)
#> 55 +/- 2

print(mean(x), digits=3)
#> 5.500 +/- 0.957
```
