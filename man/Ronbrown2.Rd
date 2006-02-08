\name{Ronbrown2}
\alias{Ronbrown2}
\docType{data}
\title{ Oceanography data obtained at site 2 by scientist aboard the ship Ron Brown}
\description{
  Data for Exercise 2.56 and Example 2.4
}
\usage{Ronbrown2}
\format{
  A data frame with 150 observations on the following 6 variables.
  \describe{
    \item{\code{depth}}{a numeric vector}
    \item{\code{primarytemp}}{a numeric vector}
    \item{\code{secondarytemp}}{a numeric vector}
    \item{\code{primarysalinity}}{a numeric vector}
    \item{\code{secondarysalinity}}{a numeric vector}
    \item{\code{density}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Ronbrown2)
attach(Ronbrown2)
plot(depth,primarysalinity,xlab="Depth",ylab="Salinity",
main="Example 2.4",col="tomato")
detach(Ronbrown2)
}
\keyword{datasets}
