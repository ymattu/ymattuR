% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{\%nin\%}
\alias{\%nin\%}
\title{inverse of \%in\%}
\usage{
x \%nin\% table
}
\arguments{
\item{x}{is the vector of values to be matched}

\item{table}{is actually a vector, to be matched against}
}
\value{
logical vector of length of x
}
\description{
borrowed from Hmisc. See %in%. Original %in% is: match(x, table,
  nomatch = 0L) > 0L
}
