# Changelog

## errors 0.4.5

CRAN release: 2026-11-03

- Fix [`print()`](https://rdrr.io/r/base/print.html) method, which now
  correctly returns the object invisibly
  ([\#64](https://github.com/r-quantities/errors/issues/64)).
- Fix `all.equal.errors()` registration
  ([@MichaelChirico](https://github.com/MichaelChirico)
  [\#67](https://github.com/r-quantities/errors/issues/67) addressing
  [\#66](https://github.com/r-quantities/errors/issues/66)).
- Operations with errors and numeric vectors no longer warn about
  coercion; comparisons no longer warn about errors being dropped
  ([\#69](https://github.com/r-quantities/errors/issues/69) addressing
  [\#68](https://github.com/r-quantities/errors/issues/68)). Also,
  probabilistic comparisons are now available via a dedicated option,
  see [`?Ops.errors`](../reference/groupGeneric.errors.md) for details.
- Fix [`str()`](https://rdrr.io/r/utils/str.html) method, which now
  strips the ‘errors’ class tag whether or not the default method adds a
  leading space, so it works with R-devel r90805

## errors 0.4.4

CRAN release: 2025-06-23

- Improve `cbind` arg name deparsing.
- Switch from `geom_errobarh()` (deprecated) to
  [`geom_errorbar()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)
  with flipped aesthetics in
  [`geom_errors()`](../reference/geom_errors.md)
  ([\#63](https://github.com/r-quantities/errors/issues/63)).

## errors 0.4.3

CRAN release: 2025-01-18

- Add option `decimals` to
  [`format()`](https://rdrr.io/r/base/format.html) method to add support
  for uncertainty with decimals in the `parenthesis` notation
  ([@alusiani](https://github.com/alusiani)
  [\#60](https://github.com/r-quantities/errors/issues/60),
  [\#61](https://github.com/r-quantities/errors/issues/61) addressing
  [\#47](https://github.com/r-quantities/errors/issues/47)).
- Fix internal `propagate` function (for r-quantities/units#371).

## errors 0.4.2

CRAN release: 2024-07-29

- Add support for PDG rounding rules
  ([@davidchall](https://github.com/davidchall)
  [\#59](https://github.com/r-quantities/errors/issues/59) addressing
  [\#45](https://github.com/r-quantities/errors/issues/45)).
- Improve formatting of null values
  ([\#48](https://github.com/r-quantities/errors/issues/48)).

## errors 0.4.1

CRAN release: 2023-12-06

- Switch from `size` (deprecated in `ggplot2` v3.4.0) to `linewidth`
  aesthetic in [`geom_errors()`](../reference/geom_errors.md)
  ([\#55](https://github.com/r-quantities/errors/issues/55)).
- Implement methods for dealing with missing values in `errors` objects,
  and fix `na.rm` behavior for summary methods
  ([\#56](https://github.com/r-quantities/errors/issues/56)).
- Performance improvements for `data.frame` methods.

## errors 0.4.0

CRAN release: 2022-12-04

- Add new [`geom_errors()`](../reference/geom_errors.md) function to
  automatically add errorbars to `ggplot2` plots for variables of class
  `errors` ([\#52](https://github.com/r-quantities/errors/issues/52)).
- Some fixes in [`plot.errors()`](../reference/plot.errors.md), improved
  `units` compatibility.
- Implement errors methods for
  [`duplicated()`](https://rdrr.io/r/base/duplicated.html),
  [`anyDuplicated()`](https://rdrr.io/r/base/duplicated.html) and
  [`unique()`](https://rdrr.io/r/base/unique.html).
- Fix operators `%/%` and `%%`
  ([\#54](https://github.com/r-quantities/errors/issues/54)).

## errors 0.3.6

CRAN release: 2020-11-10

- Fix check tolerance when correlations are approx. 1
  ([\#42](https://github.com/r-quantities/errors/issues/42)).

## errors 0.3.5

CRAN release: 2020-08-26

- Internal performance improvements
  ([\#39](https://github.com/r-quantities/errors/issues/39)).

## errors 0.3.4

CRAN release: 2020-06-13

- vctrs methods are now implemented for compatibility with dplyr 1.0
  ([\#38](https://github.com/r-quantities/errors/issues/38)).
- Implement prettier [`str()`](https://rdrr.io/r/utils/str.html) print
  ([\#36](https://github.com/r-quantities/errors/issues/36)).
- Implement drop method for data frames.

## errors 0.3.3

CRAN release: 2020-01-08

- Fix [`min()`](https://rdrr.io/r/base/Extremes.html),
  [`max()`](https://rdrr.io/r/base/Extremes.html) and
  [`range()`](https://rdrr.io/r/base/range.html)
  ([\#35](https://github.com/r-quantities/errors/issues/35)).
- Rewrite [`plot()`](https://rdrr.io/r/graphics/plot.default.html) to
  fix issues with limits and labels
  ([\#35](https://github.com/r-quantities/errors/issues/35)).

## errors 0.3.2

CRAN release: 2019-05-20

- Add CITATION for the *R Journal* paper.
- Add the paper as a vignette.

## errors 0.3.1

CRAN release: 2018-12-05

### New features

- Add a new dataset from the Annex H of the *Guide to the Expression of
  Uncertainty in Measurement* (GUM) and associated example (f6e5461, see
  [`?GUM.H.3`](../reference/datasets.md)).
- Provide methods for logical values to automatically reinterpret `NA`
  as `NA_real_`
  ([\#30](https://github.com/r-quantities/errors/issues/30)).
- Add delayed S3 registration mechanism for R \>= 3.6.0 (955e7ad).

### Minor changes and fixes

- Move GUM examples to
  [`` ?`errors-package` ``](../reference/errors-package.md) and provide
  new examples for [`?correl`](../reference/correl.md) (aed40eb).
- Simplify object ID handling
  ([\#27](https://github.com/r-quantities/errors/issues/27)).
- Fix formatting in presence of missing values (as part of
  [\#30](https://github.com/r-quantities/errors/issues/30)).
- Fix [`Summary.errors()`](../reference/groupGeneric.errors.md) to deal
  with named arguments (dd4607b).

## errors 0.3.0

CRAN release: 2018-07-26

### New features

- Implement [`as.list()`](https://rdrr.io/r/base/list.html) for `errors`
  objects (7aff546).
- Implement support for correlations between variables
  ([\#26](https://github.com/r-quantities/errors/issues/26)). The
  [`correl()`](../reference/correl.md) and
  [`covar()`](../reference/correl.md) methods set and retrieve pairwise
  correlations or covariances, respectively, between `errors` objects,
  thereby completing the full first-order Taylor series method of
  propagation of uncertainty.
- Add a new dataset from the Annex H of the *Guide to the Expression of
  Uncertainty in Measurement* (GUM) (as part of
  [\#26](https://github.com/r-quantities/errors/issues/26); see
  [`?GUM.H.2`](../reference/datasets.md) and examples under
  [`?correl`](../reference/correl.md)).
- Implement [`all.equal()`](https://rdrr.io/r/base/all.equal.html) for
  `errors` objects (as part of
  [\#26](https://github.com/r-quantities/errors/issues/26)), which
  returns `TRUE` for *different* (different internal identifier) objects
  with the same quantity values and uncertainty. To compare whether two
  variables are exactly the *same* object,
  [`identical()`](https://rdrr.io/r/base/identical.html) should be used
  instead.

### Minor changes and fixes:

- Fix encoding issues for the plus-minus symbol
  ([\#24](https://github.com/r-quantities/errors/issues/24)).
- Fix `pillar` representation (14df89b).
- Fix coercion issues (0e04519).
- Improve documentation (as part of
  [\#26](https://github.com/r-quantities/errors/issues/26)).

## errors 0.2.1

CRAN release: 2018-04-03

### New features

- New [`drop_errors()`](../reference/drop_errors.md) method drops
  `errors` class and attributes. Equivalent to setting a `NULL` error
  ([\#23](https://github.com/r-quantities/errors/issues/23)).
- Implement `pillar_shaft()` for tidy `tibble` printing and improve
  `type_sum()` (700f1aa).

### Minor changes and fixes:

- Show `Ops()` warnings once
  ([\#22](https://github.com/r-quantities/errors/issues/22)).
- Refactor [`print()`](https://rdrr.io/r/base/print.html) method
  (2c252d9).
- Drop support for matrix multiplication: no warning is issued anymore
  (090e953).
- Fix
  [`cbind()`](https://rdrr.io/r/base/cbind.html)/[`rbind()`](https://rdrr.io/r/base/cbind.html)
  column/row name parsing for named arguments (b8ea6b5).
- Improve plus-minus separator (8b3d231).
- Fix formatting to take into account the `scipen` option (9299551).

## errors 0.2.0

CRAN release: 2017-11-26

### Minor changes and fixes:

- Show a warning and drop errors in boolean operators instead of failing
  ([\#17](https://github.com/r-quantities/errors/issues/17)).
- [`min()`](https://rdrr.io/r/base/Extremes.html),
  [`max()`](https://rdrr.io/r/base/Extremes.html),
  [`range()`](https://rdrr.io/r/base/range.html) return numeric,
  including the error: value minus error, value plus error and
  `(min, max)` respectively
  ([\#18](https://github.com/r-quantities/errors/issues/18)).
- Use [`structure()`](https://rdrr.io/r/base/structure.html) instead of
  [`set_errors()`](../reference/errors.md) internally
  ([\#20](https://github.com/r-quantities/errors/issues/20)).
- Improved support for matrices and data frames with the implementation
  of [`rbind()`](https://rdrr.io/r/base/cbind.html),
  [`cbind()`](https://rdrr.io/r/base/cbind.html),
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) for
  `errors` objects
  ([\#21](https://github.com/r-quantities/errors/issues/21)).
- Improved documentation and testing
  ([\#19](https://github.com/r-quantities/errors/issues/19)).

## errors 0.1.0

CRAN release: 2017-09-26

### Minor changes and fixes:

- Improved support for arrays and matrices
  ([\#4](https://github.com/r-quantities/errors/issues/4)).
- `Inf`, `NaN` and `NA` values default to `Inf`, `NaN` and `NA` errors
  respectively
  ([\#10](https://github.com/r-quantities/errors/issues/10)).
- Drop errors with a warning in matrix multiplication
  ([\#11](https://github.com/r-quantities/errors/issues/11)).
- Fix errors defined as integers
  ([\#12](https://github.com/r-quantities/errors/issues/12)).
- Do not allow boolean operators on `errors` objects
  ([\#13](https://github.com/r-quantities/errors/issues/13)).
- Give a default value to [`set_errors()`](../reference/errors.md)
  ([\#14](https://github.com/r-quantities/errors/issues/14)).
- Tidy `tibble` printing
  ([\#15](https://github.com/r-quantities/errors/issues/15)).
- Correct plot limits according to error bars
  ([\#16](https://github.com/r-quantities/errors/issues/16)).

## errors 0.0.2

CRAN release: 2017-06-07

### Minor changes and fixes:

- Fix exponentiation with negative base
  ([\#2](https://github.com/r-quantities/errors/issues/2)).
- Fix formatting when the value equals zero
  ([\#3](https://github.com/r-quantities/errors/issues/3)).
- Fix formatting when the error equals zero
  ([\#5](https://github.com/r-quantities/errors/issues/5)).
- Take `digits` into account for errors in vector display
  ([\#7](https://github.com/r-quantities/errors/issues/7)).
- Fix deletion of trailing zeroes in plus-minus notation
  ([\#8](https://github.com/r-quantities/errors/issues/8)).
