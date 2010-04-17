\name{Vehicle}
\alias{Vehicle}
\docType{data}
\title{ Fatality ratings for foreign and domestic vehicles}
\description{
  Data for Exercise 8.34
}
\usage{Vehicle}
\format{
  A data frame with 2 observations on the following 6 variables.
  \describe{
    \item{\code{make}}{a factor with levels \code{domestic} \code{foreign}}
    \item{\code{A}}{a numeric vector}
    \item{\code{B}}{a numeric vector}
    \item{\code{C}}{a numeric vector}
    \item{\code{D}}{a numeric vector}
    \item{\code{F}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Vehicle)
attach(Vehicle)
Vehicle
chisq.test(Vehicle[,2:6])
detach(Vehicle)
}
\keyword{datasets}
