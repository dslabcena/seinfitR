% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seinfitR_control.R
\name{seinfitR_control}
\alias{seinfitR_control}
\title{SeinfitR Control}
\usage{
seinfitR_control(
  ftol = sqrt(.Machine$double.eps),
  ptol = sqrt(.Machine$double.eps),
  gtol = 0,
  diag = list(),
  epsfcn = 0,
  factor = 100,
  maxfev = integer(),
  maxiter = 50,
  nprint = 0,
  trace = FALSE
)
}
\arguments{
\item{ftol}{Termination condition for relative reduction in the sum of squares.}

\item{ptol}{Termination based on relative error between two consecutive iterations.}

\item{gtol}{Controls the orthogonality between the function vector and the Jacobian.}

\item{diag}{Multiplicative scale factors for the parameters.}

\item{epsfcn}{Step size for forward-difference approximation of the Jacobian.}

\item{factor}{Initial step bound factor.}

\item{maxfev}{Maximum number of function evaluations.}

\item{maxiter}{Maximum number of iterations.}

\item{nprint}{Controls printing of iteration details.}

\item{trace}{A logical value indicating if a trace of the iteration progress should be printed.}
}
\value{
A list of control parameters to be used in the \code{nlsLM} function during the
fitting of the Seinhorst model using \code{seinfitR}.
}
\description{
Custom Control Function for the SeinfitR Model Fitting
}
\details{
This function returns a list of control parameters for the Levenberg-Marquardt algorithm
used by the \code{nlsLM} function from the \code{minpack.lm} package. These parameters
are specifically designed to control the fitting process in the \code{seinfitR} function.
}
