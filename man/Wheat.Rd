\name{Wheat}
\alias{Wheat}
\docType{data}
\title{Price of a bushel of wheat versus the national weekly earnings of production workers}
\description{
  Data for Exercise 2.11
}
\usage{Wheat}
\format{
  A data frame with 19 observations on the following 3 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{earnings}}{a numeric vector}
    \item{\code{price}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Wheat)
attach(Wheat)
par(mfrow=c(1,2))
plot(year,earnings)
plot(year,price)
par(mfrow=c(1,1))
detach(Wheat)
}
\keyword{datasets}
