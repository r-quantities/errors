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
