\name{Press}
\alias{Press}
\docType{data}
\title{Degree of confidence in the press versus education level for 20 randomly selected persons}
\description{
  Data for Exercise 9.55
}
\usage{Press}
\format{
  A data frame with 20 observations on the following 4 variables.
  \describe{
    \item{\code{educat}}{a numeric vector}
    \item{\code{confid}}{a numeric vector}
    \item{\code{SRES1}}{a numeric vector}
    \item{\code{FITS1}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Press)
attach(Press)
summary(lm(confid~educat))
detach(Press)
}
\keyword{datasets}
