% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diff_nh.R
\name{diff_nh}
\alias{diff_nh}
\title{Differentiate x and y and compute change amplitude via an algorithm proposed by Nyström and Holmqvist (2010) \doi{10.3758/BRM.42.1.188}}
\usage{
diff_nh(x, y, trial, sample_rate, options = NULL)
}
\arguments{
\item{x}{vector with x coordinates in \emph{degrees of visual angle}}

\item{y}{vector with y coordinates in \emph{degrees of visual angle}}

\item{trial}{vector with trial index}

\item{sample_rate}{sample rate in Hz}

\item{options}{List with method specific options, see Details.}
}
\value{
\code{data.frame} with columns \code{x}, \code{y}, and \code{amp}
}
\description{
Differentiate x and y and compute change amplitude via an algorithm proposed by Nyström 
and Holmqvist (2010) \doi{10.3758/BRM.42.1.188}. Note that both components and the amplitude are smoothed
\emph{independently} via a Savitzky-Golay filter, so the components may not (probably won't) add up to
the amplitude. Note that filtering is sensitive to the presence of \code{NA}.
}
\details{
Method options, please refer to Nyström and Holmqvist (2010) for details on parameters and the rationale for default values.
\itemize{
\item{\code{nh_sg_filter_order}} {Order of Savitzky-Golay filter. Defaults to \code{2}.}
}
}
\examples{
diff_nh(rnorm(1000), rnorm(1000), rep(1, 1000), 250, list("nh_sg_filter_order" = 2))
}
\seealso{
filter_via_savitzky_golay
}
