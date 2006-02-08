\name{Reading}
\alias{Reading}
\docType{data}
\title{Standardized reading scores for 30 fifth graders}
\description{
  Data for Exercise 1.72 and 2.10
}
\usage{Reading}
\format{
  A data frame with 30 observations on the following 4 variables.
  \describe{
    \item{\code{reading}}{a numeric vector}
    \item{\code{sorted}}{a numeric vector}
    \item{\code{trimmed}}{a numeric vector}
    \item{\code{winsoriz}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Reading)
attach(Reading)
EDA(reading)
detach(Reading)
}
\keyword{datasets}
