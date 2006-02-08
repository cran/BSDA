\name{Referend}
\alias{Referend}
\docType{data}
\title{ Opinion on referendum by view on freedom of the press}
\description{
  Data for Exercise 8.20
}
\usage{Referend}
\format{
  A data frame with 3 observations on the following 4 variables.
  \describe{
    \item{\code{Response}}{a factor with levels \code{A} \code{B} \code{C}}
    \item{\code{For}}{a numeric vector}
    \item{\code{Against}}{a numeric vector}
    \item{\code{undecide}}{a numeric vector}
  }}
  
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Referend)
attach(Referend)
chisq.test(Referend[,2:4])
detach(Referend)
}
\keyword{datasets}
