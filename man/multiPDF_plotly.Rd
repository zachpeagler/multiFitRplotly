% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiFitRplotly.R
\name{multiPDF_plotly}
\alias{multiPDF_plotly}
\title{Multiple PDF Plot For Continuous Variables
Using Plotly}
\usage{
multiPDF_plotly(x, seq_length, distributions)
}
\arguments{
\item{x}{The variable to for which to plot PDFs}

\item{seq_length}{The number of points over which to fit x}

\item{distributions}{The distributions to fit x against}
}
\value{
A dataframe with x, the real density, and the pdf of the desired
distributions with length(nrow) equal to seq_length +1.
}
\description{
This function returns a plotly output showing the PDFs for selected distributions
against a continuous, non-negative input variable. Possible distributions include "normal",
"lognormal", "gamma", "exponential", "cauchy", "t", "weibull", "logistic",
and "all".
}
