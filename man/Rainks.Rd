\name{Rainks}
\alias{Rainks}
\docType{data}
\title{Rainfall in an area of west central Kansas and four surrounding counties}
\description{
  Data for Exercise 9.8
}
\usage{Rainks}
\format{
  A data frame with 35 observations on the following 5 variables.
  \describe{
    \item{\code{rain}}{a numeric vector}
    \item{\code{x1}}{a numeric vector}
    \item{\code{x2}}{a numeric vector}
    \item{\code{x3}}{a numeric vector}
    \item{\code{x4}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Rainks)
attach(Rainks)
cor(Rainks)
lm(rain~x2)
detach(Rainks)
}
\keyword{datasets}
