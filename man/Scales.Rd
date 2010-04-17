\name{Scales}
\alias{Scales}
\docType{data}
\title{Readings obtained from a 100 pound weight placed on four brands of bathroom scales}
\description{
  Data for Exercise 1.89
}
\usage{Scales}
\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{Brand}}{a factor with levels \code{A} \code{B} \code{C} \code{D}}
    \item{\code{reading}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Scales)
attach(Scales)
boxplot(reading~Brand,ylab="Reading",xlab="Brand",main="Problem 1.89")
detach(Scales)
}
\keyword{datasets}
