% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.analysis.R
\name{fit.hill.model}
\alias{fit.hill.model}
\title{Fits a hill model using \code{nls}. The formula for the hill model is \eqn{\frac{1}{1+\frac{c}{EC_{50}^n}}.}
\usage{
fit.hill.model(
  data,
  response.variable.name = "response",
  guesses = list(ec50 = 1, n = 1),
  lower = list(ec50 = 0, n = 0),
  print.summary = TRUE,
  algorithm = "default",
  trace = FALSE,
  control = nls.control()
)
}
\arguments{
\item{data}{A data frame providing the data, to which the model will be fitted. A column with concentrations (named concentrations) is required. It should contain in the same unit the concentrations for each measurement.}
}
\value{
A model object (nlsModel) fitted to the data provided.
}
\description{
Fits a hill model using \code{nls}. The formula for the hill model is \eqn{\frac{1}{1+\frac{c}{EC_{50}^n}}.
}
