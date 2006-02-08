\name{Repair}
\alias{Repair}
\docType{data}
\title{ Recorded times for repairing 22 automobiles involved in wrecks}
\description{
  Data for Exercise 5.77
}
\usage{Repair}
\format{
  A data frame with 22 observations on the following variable.
  \describe{
    \item{\code{time}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Repair)
attach(Repair)
stem(time)
sign.test(time,conf.level=.98)
detach(Repair)
}
\keyword{datasets}
