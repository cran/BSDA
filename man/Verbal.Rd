\name{Verbal}
\alias{Verbal}
\docType{data}
\title{ Verbal test scores and number of library books checked out for 15 eighth graders}
\description{
  Data for Exercise 9.30
}
\usage{Verbal}
\format{
  A data frame with 15 observations on the following 4 variables.
  \describe{
    \item{\code{number}}{a numeric vector}
    \item{\code{verbal}}{a numeric vector}
    \item{\code{SRES1}}{a numeric vector}
    \item{\code{FITS1}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Verbal)
attach(Verbal)
model <- lm(verbal~number)
summary(model)
detach(Verbal)
remove(model)
}
\keyword{datasets}
