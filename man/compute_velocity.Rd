% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_velocity.R
\name{compute_velocity}
\alias{compute_velocity}
\title{Compute velocity based on Engbert & Kliegl (2003)}
\usage{
compute_velocity(x, trial, sample_rate, velocity_time_window)
}
\arguments{
\item{x}{Vector of values}

\item{trial}{Vector of trial indexes.}

\item{sample_rate}{Sample rate of the recording in Hz.}

\item{velocity_time_window}{Time window in milliseconds relative to the sample that is used  to compute the velocity.}
}
\value{
vector of float
}
\description{
Compute velocity based on Engbert & Kliegl (2003)
}
\examples{
compute_velocity(rnorm(100), rep(1, 100), 500, 20)
}
