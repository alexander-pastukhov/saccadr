% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_that_dimensions_match.R
\name{check_that_lengths_match}
\alias{check_that_lengths_match}
\title{Checks that all vectors in the list have the same length.}
\usage{
check_that_lengths_match(list_of_vectors)
}
\arguments{
\item{list_of_vectors}{}
}
\description{
Checks that all vectors in the list have the same length.
Raises an error, if that is not the case.
}
\examples{
check_that_lengths_match(list(x = c(1, 2, 3), y = c(4, 5, 6)))
}
\keyword{internal}
