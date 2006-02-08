\name{Snore}
\alias{Snore}
\docType{data}
\title{ Snoring versus heart disease}
\description{
  Data for Exercise 8.21
}
\usage{Snore}
\format{
  A data frame with 2 observations on the following 5 variables.
  \describe{
    \item{\code{heart}}{a factor with levels \code{no} \code{yes}}
    \item{\code{Non}}{a numeric vector}
    \item{\code{occasion}}{a numeric vector}
    \item{\code{nearly}}{a numeric vector}
    \item{\code{every}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Snore)
attach(Snore)
chisq.test(Snore[,2:5])
detach(Snore)
}
\keyword{datasets}
