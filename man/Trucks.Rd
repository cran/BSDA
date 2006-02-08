\name{Trucks}
\alias{Trucks}
\docType{data}
\title{ Miles per gallon for standard 4-wheel drive trucks manufactured by Chevrolet, Dodge and Ford}
\description{
  Data for Example 10.2
}
\usage{Trucks}
\format{
  A data frame with 15 observations on the following 5 variables.
  \describe{
    \item{\code{chevy}}{a numeric vector}
    \item{\code{dodge}}{a numeric vector}
    \item{\code{ford}}{a numeric vector}
    \item{\code{gas.mileage}}{a numeric vector}
    \item{\code{truck}}{a factor with levels \code{chevy} \code{dodge} \code{ford}}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Trucks)
attach(Trucks)
anova(lm(gas.mileage~truck))
detach(Trucks)
}
\keyword{datasets}
