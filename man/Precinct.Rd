\name{Precinct}
\alias{Precinct}
\docType{data}
\title{Robbery rates versus percent low income in 8 precincts}
\description{
  Data for Exercise 2.2 and 2.38
}
\usage{Precinct}
\format{
  A data frame with 8 observations on the following 2 variables.
  \describe{
    \item{\code{rate}}{a numeric vector}
    \item{\code{income}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Precinct)
attach(Precinct)
plot(rate,income,main="Exercise 2.2")
model <- lm(income~rate)
model
abline(model,col="green")
detach(Precinct)
}
\keyword{datasets}
