\name{Pollutio}
\alias{Pollutio}
\docType{data}
\title{Air pollution index for 15 randomly selected days for a major western city}
\description{
  Data for Exercise 5.59
}
\usage{Pollutio}
\format{
  A data frame with 15 observations on the following variable.
  \describe{
    \item{\code{inde}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Pollutio)
attach(Pollutio)
EDA(inde)
t.test(inde,conf.level=.98)$conf
detach(Pollutio)
}
\keyword{datasets}
