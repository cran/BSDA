\name{South}
\alias{South}
\docType{data}
\title{Murder rates for 30 cities in the South}
\description{
  Data for Exercise 1.84
}
\usage{South}
\format{
  A data frame with 31 observations on the following variable.
  \describe{
    \item{\code{rate}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(South)
attach(South)
boxplot(rate,col="yellow",main="Problem 1.83")
detach(South)
}
\keyword{datasets}
