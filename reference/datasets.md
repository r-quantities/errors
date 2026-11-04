# Datasets from the Guide to the Expression of Uncertainty in Measurement (GUM)

Datasets found in Annex H of the GUM (see reference below).

## Usage

``` r
GUM.H.2

GUM.H.3
```

## Format

`GUM.H.2`, from Section 2 of Annex H (Table H.2), provides simultaneous
resistance and reactance measurements. It is a data frame with 5 rows
and 3 variables:

- V:

  Voltage amplitude, in Volts.

- I:

  Current amplitude, in Amperes.

- phi:

  Phase-shift angle of the voltage relative to the current, in radians.

`GUM.H.3`, from Section 3 of Annex H (Table H.6), provides thermometer
readings and observed corrections to obtain a linear calibration curve
for some reference temperature. It is a data frame with 11 rows and 2
variables:

- tk:

  Thermometer reading, in Celsius degrees.

- bk:

  Observed correction, in Celsius degrees.

An object of class `data.frame` with 11 rows and 2 columns.

## Source

BIPM, IEC, IFCC, ILAC, IUPAC, IUPAP, ISO, and OIML (2008). Evaluation of
Measurement Data – Guide to the Expression of Uncertainty in
Measurement, 1st edn. JCGM 100:2008. *Joint Committee for Guides in
Metrology.* <https://www.bipm.org/en/committees/jc/jcgm/publications>

## See also

See [`errors-package`](errors-package.md) for examples.
