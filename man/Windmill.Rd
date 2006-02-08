\name{Windmill}
\alias{Windmill}
\docType{data}
\title{ Direct current produced by different wind velocities}
\description{
  Data for Exercise 9.34
}
\usage{Windmill}
\format{
  A data frame with 25 observations on the following 7 variables.
  \describe{
    \item{\code{velocity}}{a numeric vector}
    \item{\code{output}}{a numeric vector}
    \item{\code{SRES1}}{a numeric vector}
    \item{\code{FITS1}}{a numeric vector}
    \item{\code{X1.velocity}}{a numeric vector}
    \item{\code{SRES2}}{a numeric vector}
    \item{\code{FITS2}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Windmill)
attach(Windmill)
summary(lm(output~velocity))
detach(Windmill)
}
\keyword{datasets}
