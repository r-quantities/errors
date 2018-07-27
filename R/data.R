#' Datasets from the Guide to the Expression of Uncertainty in Measurement (GUM)
#'
#' Datasets found in Annex H of the GUM (see reference below).
#'
#' @format \code{GUM.H.2}, from Section 2 of Annex H (Table H.2), provides
#' simultaneous resistance and reactance measurements. It is a data frame with 5
#' rows and 3 variables:
#' \describe{
#'   \item{V}{Voltage amplitude, in Volts.}
#'   \item{I}{Current amplitude, in Amperes.}
#'   \item{phi}{Phase-shift angle of the voltage relative to the current, in radians.}
#' }
#' \code{GUM.H.3}, from Section 3 of Annex H (Table H.6), provides
#' thermometer readings and observed corrections to obtain a linear calibration
#' curve for some reference temperature. It is a data frame with 11 rows and
#' 2 variables:
#' \describe{
#'   \item{tk}{Thermometer reading, in Celsius degrees.}
#'   \item{bk}{Observed correction, in Celsius degrees.}
#' }
#'
#' @seealso See \code{\link{errors-package}} for examples.
#'
#' @source BIPM, IEC, IFCC, ILAC, IUPAC, IUPAP, ISO, and OIML (2008).
#' Evaluation of Measurement Data -- Guide to the Expression of Uncertainty in
#' Measurement, 1st edn. JCGM 100:2008.
#' \emph{Joint Committee for Guides in Metrology.}
#' \url{https://www.bipm.org/en/publications/guides/gum.html}
#'
#' @name datasets
"GUM.H.2"

#' @name datasets
"GUM.H.3"
