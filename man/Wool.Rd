\name{Wool}
\alias{Wool}
\docType{data}
\title{Strength tests of two types of wool fabric}
\description{
  Data for Exercise 7.42
}
\usage{Wool}
\format{
  A data frame with 10 observations on the following 2 variables.
  \describe{
    \item{\code{Type.1}}{a numeric vector}
    \item{\code{Type.2}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Wool)
attach(Wool)
t.test(Type.1,Type.2,var.equal=TRUE)
detach(Wool)
}
\keyword{datasets}
