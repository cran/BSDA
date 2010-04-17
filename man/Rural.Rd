\name{Rural}
\alias{Rural}
\docType{data}
\title{Social adjustment scores for a rural group and a city group of children}
\description{
  Data for Exercise 7.16
}
\usage{Rural}
\format{
  A data frame with 33 observations on the following 4 variables.
  \describe{
    \item{\code{Rural}}{a numeric vector}
    \item{\code{City}}{a numeric vector}
    \item{\code{score}}{a numeric vector}
    \item{\code{code}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Rural)
attach(Rural)
wilcox.test(score~code)
wilcox.test(Rural,City)
detach(Rural)
}
\keyword{datasets}
