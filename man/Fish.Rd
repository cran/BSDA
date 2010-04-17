\name{Fish}
\alias{Fish}
\docType{data}
\title{Length and number of fish caught with small and large mesh codend }
\description{
  Data for Exercises 5.83, 5.119, and 7.29
}
\usage{Fish}
\format{
  A data frame with 767 observations on the following 5 variables.
  \describe{
    \item{\code{length}}{a numeric vector}
    \item{\code{smalmesh}}{a numeric vector}
    \item{\code{largmesh}}{a numeric vector}
    \item{\code{smallmesh}}{a numeric vector}
    \item{\code{largemesh}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Fish)
attach(Fish)
median(smallmesh,na.rm=TRUE)
median(largemesh)
IQR(smallmesh,na.rm=TRUE)
IQR(largemesh)
SIGN.test(smallmesh,conf.level=.99)
SIGN.test(largemesh,conf.level=.99)
t.test(smallmesh,largemesh)
detach(Fish)
}
\keyword{datasets}
