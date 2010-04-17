\name{Retail}
\alias{Retail}
\docType{data}
\title{ Length of employment versus gross sales for 10 employees of a large retail store}
\description{
  Data for Exercise 9.59
}
\usage{Retail}
\format{
  A data frame with 10 observations on the following 2 variables.
  \describe{
    \item{\code{months}}{a numeric vector}
    \item{\code{sales}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Retail)
attach(Retail)
summary(lm(sales~months))
detach(Retail)
}
\keyword{datasets}
