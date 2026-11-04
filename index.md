# ![](https://avatars1.githubusercontent.com/u/32303769?s=40&v=4) Measurement Errors for R

The **errors** package provides support for measurement errors in R
vectors, matrices and arrays: automatic uncertainty propagation and
reporting.

## Documentation

See [the initial blog
post](https://enchufa2.es/archives/errors-0-0-1.html). The package
includes a vignette that is an up-to-date version of the following R
Journal publication, which should be used for citations:

- Iñaki Ucar, Edzer Pebesma and Arturo Azcorra (2018). “Measurement
  Errors in R.” *The R Journal*, 10 (2), 549–557. DOI:
  [10.32614/RJ-2018-075](https://doi.org/10.32614/RJ-2018-075)

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
