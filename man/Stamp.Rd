\name{Stamp}
\alias{Stamp}
\docType{data}
\title{ Thicknesses of 1872 Hidalgo stamps issued in Mexico }
\description{
  Data for Statistical Insight Chapter 1 and Exercise 5.110
}
\usage{Stamp}
\format{
  A data frame with 485 observations on the following 3 variables.
  \describe{
    \item{\code{thickness}}{a numeric vector}
    \item{\code{thick}}{a numeric vector}
    \item{\code{freq}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Stamp)
attach(Stamp)
hist(thickness,prob=TRUE,col="lightblue")
lines(density(thickness),lwd=2,col="blue")
t.test(thickness,conf.level=.99)$conf
detach(Stamp)
}
\keyword{datasets}
