% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{format.errors}
\alias{format.errors}
\title{Encode \code{errors}}
\usage{
\method{format}{errors}(x, digits = NULL, scientific = FALSE,
  notation = getOption("errors.notation", "parenthesis"),
  decimals = getOption("errors.decimals", FALSE), ...)
}
\arguments{
\item{x}{an \code{errors} object.}

\item{digits}{how many significant digits are to be used for uncertainties.
The default, \code{NULL}, uses \code{getOption("errors.digits", 1)}.
Use \code{digits="pdg"} to choose an appropriate number of digits for each
value according to the Particle Data Group rounding rule (see references).}

\item{scientific}{logical specifying whether the elements should be
encoded in scientific format.}

\item{notation}{error notation; \code{"parenthesis"} and \code{"plus-minus"}
are supported through the \code{"errors.notation"} option.}

\item{decimals}{logical specifying whether the uncertainty should be formatted
with a decimal point even when the \code{"parenthesis"} notation is used.
Otherwise (by default), the \code{"parenthesis"} notation scales the
uncertainty to match the least significant digit of the value.}

\item{...}{ignored.}
}
\description{
Format an \code{errors} object for pretty printing.
}
\examples{
x <- set_errors(1:3*100, 1:3*100 * 0.05)
format(x)
format(x, digits=2)
format(x, digits=2, decimals=TRUE)
format(x, scientific=TRUE)
format(x, notation="plus-minus")

x <- set_errors(c(0.827, 0.827), c(0.119, 0.367))
format(x, notation="plus-minus", digits="pdg")

}
\references{
K. Nakamura et al. (Particle Data Group), J. Phys. G 37, 075021 (2010)
}
