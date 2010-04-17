\name{Ronbrown1}
\alias{Ronbrown1}
\docType{data}
\title{ Oceanography data obtained at site 1 by scientist aboard the ship Ron Brown}
\description{
  Data for Exercise 2.9
}
\usage{Ronbrown1}
\format{
  A data frame with 75 observations on the following 12 variables.
  \describe{
    \item{\code{depth}}{a numeric vector}
    \item{\code{downtemp1}}{a numeric vector}
    \item{\code{downtemp2}}{a numeric vector}
    \item{\code{downsalinity1}}{a numeric vector}
    \item{\code{downsalinity2}}{a numeric vector}
    \item{\code{downdensity}}{a numeric vector}
    \item{\code{C7}}{a numeric vector}
    \item{\code{uptemp1}}{a numeric vector}
    \item{\code{uptemp2}}{a numeric vector}
    \item{\code{upsalinity1}}{a numeric vector}
    \item{\code{upsalinity2}}{a numeric vector}
    \item{\code{updensity}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Ronbrown1)
attach(Ronbrown1)
plot(depth,downtemp1)
detach(Ronbrown1)
}
\keyword{datasets}
