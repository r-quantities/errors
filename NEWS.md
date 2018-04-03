# errors 0.2.1

## New features

* New `drop_errors` method drops `errors` class and attributes. Equivalent to setting a `NULL` error (#23).
* Implement `pillar_shaft` for tidy `tibble` printing and improve `type_sum` (700f1aa).

## Minor changes and fixes:

* Show `Ops` warnings once (#22).
* Refactor `print` method (2c252d9).
* Drop support for matrix multiplication: no warning is issued anymore (090e953).
* Fix `cbind`/`rbind` column/row name parsing for named arguments (b8ea6b5).
* Improve plus-minus separator (8b3d231).
* Fix formatting to take into account the `scipen` option (9299551).

# errors 0.2.0

## Minor changes and fixes:

* Show a warning and drop errors in boolean operators instead of failing (#17).
* `min`, `max`, `range` return numeric, including the error: value minus error, value plus error and `(min, max)` respectively (#18).
* Use `structure` instead of `set_errors` internally (#20).
* Improved support for matrices and data frames with the implementation of `rbind`, `cbind`, `as.data.frame` for `errors` objects (#21).
* Improved documentation and testing (#19).

# errors 0.1.0

## Minor changes and fixes:

* Improved support for arrays and matrices (#4).
* `Inf`, `NaN` and `NA` values default to `Inf`, `NaN` and `NA` errors respectively (#10).
* Drop errors with a warning in matrix multiplication (#11).
* Fix errors defined as integers (#12).
* Do not allow boolean operators on `errors` objects (#13).
* Give a default value to `set_errors` (#14).
* Tidy `tibble` printing (#15).
* Correct plot limits according to error bars (#16).

# errors 0.0.2

## Minor changes and fixes:

* Fix exponentiation with negative base (#2).
* Fix formatting when the value equals zero (#3).
* Fix formatting when the error equals zero (#5).
* Take `digits` into account for errors in vector display (#7).
* Fix deletion of trailing zeroes in plus-minus notation (#8).
