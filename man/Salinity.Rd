\name{Salinity}
\alias{Salinity}
\docType{data}
\title{Surface-water salinity measurements from Whitewater Bay, Florida}
\description{
  Data for Exercise 5.27 and 5.64
}
\usage{Salinity}
\format{
  A data frame with 48 observations on the following variable.
  \describe{
    \item{\code{salinity}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Salinity)
attach(Salinity)
EDA(salinity)
t.test(salinity,conf.level=.99)$conf
detach(Salinity)
}
\keyword{datasets}
