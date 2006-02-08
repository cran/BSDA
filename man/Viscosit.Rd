\name{Viscosit}
\alias{Viscosit}
\docType{data}
\title{Viscosity measurements of a substance on two different days}
\description{
  Data for Exercise 7.44
}
\usage{Viscosit}
\format{
  A data frame with 11 observations on the following 2 variables.
  \describe{
    \item{\code{first}}{a numeric vector}
    \item{\code{second}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Viscosit)
attach(Viscosit)
t.test(first,second)
detach(Viscosit)
}
\keyword{datasets}
