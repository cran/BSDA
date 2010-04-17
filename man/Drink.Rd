\name{Drink}
\alias{Drink}
\docType{data}
\title{Opinion on referendum by view on moral issue of selling alcoholic beverages}
\description{
  Data for Exercise 8.53
}
\usage{Drink}
\format{
  A data frame with 3 observations on the following 4 variables.
  \describe{
    \item{\code{drink}}{a factor with levels \code{immoral} \code{ok} \code{tolerated}}
    \item{\code{For}}{a numeric vector}
    \item{\code{Against}}{a numeric vector}
    \item{\code{undecide}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Drink)
attach(Drink)
Drink
chisq.test(Drink[,2:4])
detach(Drink)
}
\keyword{datasets}
