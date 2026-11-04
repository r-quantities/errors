# errors: Uncertainty Propagation for R Vectors

Support for measurement errors in R vectors, matrices and arrays:
automatic uncertainty propagation and reporting.

## Details

Every measurement has an unknown error associated. Uncertainty is the
acknowledgement of that error: we are aware that our representation of
reality may differ from reality itself. This package provides support
for measurement errors in R vectors, matrices and arrays. Uncertainty
metadata is associated to quantity values (see [`errors`](errors.md)),
and this uncertainty is automatically propagated when you operate with
`errors` objects (see [`groupGeneric.errors`](groupGeneric.errors.md)),
or with `errors` and numeric objects (then numeric values are
automatically coerced to `errors` objects with no uncertainty).

Correlations between measurements are also supported. In particular, any
operation (e.g., `z <- x + y`) results in a correlation between output
and input variables (i.e., `z` is correlated to `x` and `y`, even if
there was no correlation between `x` and `y`). And in general, the user
can establish correlations between any pair of variables (see
[`correl`](correl.md)).

This package treats uncertainty as coming from Gaussian and linear
sources (note that, even for non-Gaussian non-linear sources, this is a
reasonable assumption for averages of many measurements), and propagates
them using the first-order Taylor series method for propagation of
uncertainty. Although the above assumptions are valid in a wide range of
applications in science and engineering, the practitioner should
evaluate whether they apply for each particular case.

## References

Iñaki Ucar, Edzer Pebesma and Arturo Azcorra (2018). Measurement Errors
in R. *The R Journal*, 10(2), 549-557.
[doi:10.32614/RJ-2018-075](https://doi.org/10.32614/RJ-2018-075)

## See also

[`datasets`](datasets.md) for a description of the datasets used in the
examples below.

## Author

Iñaki Ucar

## Examples

``` r
## Simultaneous resistance and reactance measurements

# Obtain mean values and uncertainty from measured values
V   <- mean(set_errors(GUM.H.2$V))
I   <- mean(set_errors(GUM.H.2$I))
phi <- mean(set_errors(GUM.H.2$phi))

# Set correlations between variables
correl(V, I)   <- with(GUM.H.2, cor(V, I))
correl(V, phi) <- with(GUM.H.2, cor(V, phi))
correl(I, phi) <- with(GUM.H.2, cor(I, phi))

# Computation of resistance, reactance and impedance values
(R <- (V / I) * cos(phi))
#> 127.73(7)
(X <- (V / I) * sin(phi))
#> 219.8(3)
(Z <- (V / I))
#> 254.3(2)

# Correlations between derived quantities
correl(R, X)
#> [1] -0.5884298
correl(R, Z)
#> [1] -0.4852592
correl(X, Z)
#> [1] 0.9925116

## Calibration of a thermometer

# Least-squares fit for a reference temperature of 20 degC
fit <- lm(bk ~ I(tk - 20), data = GUM.H.3)

# Extract coefficients and set correlation using the covariance matrix
y1 <- set_errors(coef(fit)[1], sqrt(vcov(fit)[1, 1]))
y2 <- set_errors(coef(fit)[2], sqrt(vcov(fit)[2, 2]))
covar(y1, y2) <- vcov(fit)[1, 2]

# Predicted correction for 30 degC
(b.30 <- y1 + y2 * set_errors(30 - 20))
#> -0.149(4)
```
