\name{Window}
\alias{Window}
\docType{data}
\title{Wind leakage for storm windows exposed to a 50 mph wind}
\description{
  Data for Exercise 6.54
}
\usage{Window}
\format{
  A data frame with 9 observations on the following 2 variables.
  \describe{
    \item{\code{Window}}{a numeric vector}
    \item{\code{Leakage}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Window)
attach(Window)
sign.test(Leakage,md=.125,alternative="greater")
detach(Window)
}
\keyword{datasets}
