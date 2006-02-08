\name{Orange}
\alias{Orange}
\docType{data}
\title{Price of oranges versus size of the harvest}
\description{
  Data for Exercise 9.61
}
\usage{Orange}
\format{
  A data frame with 6 observations on the following 2 variables.
  \describe{
    \item{\code{harvest}}{a numeric vector}
    \item{\code{price}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Orange)
attach(Orange)
summary(lm(price~harvest))
detach(Orange)
}
\keyword{datasets}
