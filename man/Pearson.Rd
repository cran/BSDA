\name{Pearson}
\alias{Pearson}
\docType{data}
\title{Karl Pearson's data on heights of brothers and sisters}
\description{
  Data for Exercise 2.20
}
\usage{Pearson}
\format{
  A data frame with 11 observations on the following 2 variables.
  \describe{
    \item{\code{brother}}{a numeric vector}
    \item{\code{sister}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Pearson)
attach(Pearson)
plot(brother,sister)
cor(brother,sister)
detach(Pearson)
}
\keyword{datasets}
