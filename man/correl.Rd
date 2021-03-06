% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correl.R
\name{correl}
\alias{correl}
\alias{correl<-}
\alias{set_correl}
\alias{covar}
\alias{covar<-}
\alias{set_covar}
\title{Handle Correlations Between \code{errors} Objects}
\usage{
correl(x, y)

correl(x, y) <- value

set_correl(x, y, value)

covar(x, y)

covar(x, y) <- value

set_covar(x, y, value)
}
\arguments{
\item{x}{an object of class \code{errors}.}

\item{y}{an object of class \code{errors}.}

\item{value}{a numeric vector of length 1 or the same length as \code{x}.}
}
\value{
\code{correl} and \code{covar} return a vector of correlations and
covariances respectively (or \code{NULL}).
\code{set_correl} and \code{set_covar}, which are pipe-friendly versions of
the setters, return the \code{x} object.
}
\description{
Set or retrieve correlations or covariances between \code{errors} objects.
See the details section below.
}
\details{
The uncertainties associated to \code{errors} objects are supposed
to be independent by default. If there is some known correlation, it can be
defined using these methods, and it will be used for the propagation of the
uncertainty by the mathematical and arithmetic operations.

The \code{correl} method sets or retrieves correlations, i.e., a value (or
vector of values) between \code{-1} and \code{1} (see base \code{\link{cor}}
on how to compute correlations). A covariance is just a correlation value
multiplied by the standard deviations (i.e., the standard uncertainty) of
both variables. It can be defined using the \code{covar} method (see base
\code{\link{cov}} on how to compute covariances). These methods are
equivalent; in fact, \code{correl} calls \code{covar} internally.

Every \code{errors} object has a unique ID, and pairwise correlations are
stored in an internal hash table. All the functions or methods that modify
somehow the dimensions of \code{errors} objects (i.e., subsets, binds,
concatenations, summaries...) generate new objects with new IDs, and
correlations are not, and cannot be, propagated. Only mathematical and
arithmetic operations propagate correlations, where appropriate, following
the Taylor series method.
}
\examples{
x <- set_errors(1:5, 0.1)
y <- set_errors(1:5, 0.1)

# Self-correlation is of course 1, and cannot be changed
correl(x, x)
\dontrun{
correl(x, x) <- 0.5}

# Cross-correlation can be set, but must be a value between -1 and 1
correl(x, y)
\dontrun{
correl(x, y) <- 1.5}
correl(x, y) <- runif(length(x))
correl(x, y)
covar(x, y)

}
