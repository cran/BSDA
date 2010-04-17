\name{Statclas}
\alias{Statclas}
\docType{data}
\title{ Grades for two introductory statistics classes}
\description{
  Data for Exercise 7.30
}
\usage{Statclas}
\format{
  A data frame with 36 observations on the following 2 variables.
  \describe{
    \item{\code{X9am}}{a numeric vector}
    \item{\code{X2pm}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Statclas)
attach(Statclas)
t.test(X9am,X2pm)
detach(Statclas)
}
\keyword{datasets}
