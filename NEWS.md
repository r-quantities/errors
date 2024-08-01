# errors devel

- Add option `decimals` to `format()` method to add support for uncertainty with
  decimals in the `parenthesis` notation (@alusiani #60, #61 addressing #47).

# errors 0.4.2

- Add support for PDG rounding rules (@davidchall #59 addressing #45).
- Improve formatting of null values (#48).

# errors 0.4.1

- Switch from `size` (deprecated in `ggplot2` v3.4.0) to `linewidth` aesthetic
  in `geom_errors()` (#55).
- Implement methods for dealing with missing values in `errors` objects,
  and fix `na.rm` behavior for summary methods (#56).
- Performance improvements for `data.frame` methods.

# errors 0.4.0

- Add new `geom_errors()` function to automatically add errorbars to `ggplot2`
  plots for variables of class `errors` (#52).
- Some fixes in `plot.errors()`, improved `units` compatibility.
- Implement errors methods for `duplicated()`, `anyDuplicated()` and `unique()`.
- Fix operators `%/%` and `%%` (#54).

# errors 0.3.6

- Fix check tolerance when correlations are approx. 1 (#42).

# errors 0.3.5

- Internal performance improvements (#39).

# errors 0.3.4

- vctrs methods are now implemented for compatibility with dplyr 1.0 (#38).
- Implement prettier `str()` print (#36).
- Implement drop method for data frames.

# errors 0.3.3

- Fix `min()`, `max()` and `range()` (#35).
- Rewrite `plot()` to fix issues with limits and labels (#35).

# errors 0.3.2

- Add CITATION for the *R Journal* paper.
- Add the paper as a vignette.

# errors 0.3.1

## New features

- Add a new dataset from the Annex H of the *Guide to the Expression of
  Uncertainty in Measurement* (GUM) and associated example (f6e5461, see `?GUM.H.3`).
- Provide methods for logical values to automatically reinterpret `NA` as
  `NA_real_` (#30).
- Add delayed S3 registration mechanism for R >= 3.6.0 (955e7ad).

## Minor changes and fixes

- Move GUM examples to ``?`errors-package` `` and provide new examples for
  `?correl` (aed40eb).
- Simplify object ID handling (#27).
- Fix formatting in presence of missing values (as part of #30).
- Fix `Summary.errors()` to deal with named arguments (dd4607b).

# errors 0.3.0

## New features

- Implement `as.list()` for `errors` objects (7aff546).
- Implement support for correlations between variables (#26). The `correl()` and
  `covar()` methods set and retrieve pairwise correlations or covariances,
  respectively, between `errors` objects, thereby completing the full
  first-order Taylor series method of propagation of uncertainty.
- Add a new dataset from the Annex H of the *Guide to the Expression of
  Uncertainty in Measurement* (GUM) (as part of #26; see `?GUM.H.2` and examples
  under `?correl`).
- Implement `all.equal()` for `errors` objects (as part of #26), which returns
  `TRUE` for *different* (different internal identifier) objects with the same
  quantity values and uncertainty. To compare whether two variables are exactly
  the *same* object, `identical()` should be used instead.

## Minor changes and fixes:

- Fix encoding issues for the plus-minus symbol (#24).
- Fix `pillar` representation (14df89b).
- Fix coercion issues (0e04519).
- Improve documentation (as part of #26).

# errors 0.2.1

## New features

- New `drop_errors()` method drops `errors` class and attributes. Equivalent to
  setting a `NULL` error (#23).
- Implement `pillar_shaft()` for tidy `tibble` printing and improve `type_sum()`
  (700f1aa).

## Minor changes and fixes:

- Show `Ops()` warnings once (#22).
- Refactor `print()` method (2c252d9).
- Drop support for matrix multiplication: no warning is issued anymore (090e953).
- Fix `cbind()`/`rbind()` column/row name parsing for named arguments (b8ea6b5).
- Improve plus-minus separator (8b3d231).
- Fix formatting to take into account the `scipen` option (9299551).

# errors 0.2.0

## Minor changes and fixes:

- Show a warning and drop errors in boolean operators instead of failing (#17).
- `min()`, `max()`, `range()` return numeric, including the error: value minus
  error, value plus error and `(min, max)` respectively (#18).
- Use `structure()` instead of `set_errors()` internally (#20).
- Improved support for matrices and data frames with the implementation of
  `rbind()`, `cbind()`, `as.data.frame()` for `errors` objects (#21).
- Improved documentation and testing (#19).

# errors 0.1.0

## Minor changes and fixes:

- Improved support for arrays and matrices (#4).
- `Inf`, `NaN` and `NA` values default to `Inf`, `NaN` and `NA` errors
  respectively (#10).
- Drop errors with a warning in matrix multiplication (#11).
- Fix errors defined as integers (#12).
- Do not allow boolean operators on `errors` objects (#13).
- Give a default value to `set_errors()` (#14).
- Tidy `tibble` printing (#15).
- Correct plot limits according to error bars (#16).

# errors 0.0.2

## Minor changes and fixes:

- Fix exponentiation with negative base (#2).
- Fix formatting when the value equals zero (#3).
- Fix formatting when the error equals zero (#5).
- Take `digits` into account for errors in vector display (#7).
- Fix deletion of trailing zeroes in plus-minus notation (#8).
