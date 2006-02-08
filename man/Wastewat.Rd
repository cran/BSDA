\name{Wastewat}
\alias{Wastewat}
\docType{data}
\title{Volume of injected waste water from Rocky Mountain Arsenal and number of earthquakes near Denver}
\description{
  Data for Exercise 9.18
}
\usage{Wastewat}
\format{
  A data frame with 44 observations on the following 4 variables.
  \describe{
    \item{\code{gallons}}{a numeric vector}
    \item{\code{number}}{a numeric vector}
    \item{\code{ln.no.}}{a numeric vector}
    \item{\code{index}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Wastewat)
attach(Wastewat)
model <- lm(number~gallons)
summary(model)
detach(Wastewat)
remove(model)
}
\keyword{datasets}
