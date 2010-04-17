\name{Stable}
\alias{Stable}
\docType{data}
\title{ Times of a 2-year old stallion on a one mile run}
\description{
  Data for Exercise 6.93
}
\usage{Stable}
\format{
  A data frame with 9 observations on the following variable.
  \describe{
    \item{\code{time}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Stable)
attach(Stable)
EDA(time)
SIGN.test(time,md=98.5,alternative="greater")
detach(Stable)
}
\keyword{datasets}
