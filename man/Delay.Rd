\name{Delay}
\alias{Delay}
\docType{data}
\title{Delay times on 20 flights from four major air carriers}
\description{
  Data for Exercise 10.55
}
\usage{Delay}
\format{
  A data frame with 80 observations on the following 6 variables.
  \describe{
    \item{\code{CarrierA}}{a numeric vector}
    \item{\code{CarrierB}}{a numeric vector}
    \item{\code{CarrierC}}{a numeric vector}
    \item{\code{CarrierD}}{a numeric vector}
    \item{\code{delay}}{a numeric vector}
    \item{\code{Carrier}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Delay)
attach(Delay)
boxplot(delay~Carrier)
kruskal.test(delay~as.factor(Carrier))
detach(Delay)
}
\keyword{datasets}
