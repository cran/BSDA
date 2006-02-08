\name{Inmate}
\alias{Inmate}
\docType{data}
\title{Type of drug offense by race}
\description{
  Data for Statistical Insight Chapter 8
}
\usage{Inmate}
\format{
  A data frame with 3 observations on the following 5 variables.
  \describe{
    \item{\code{Race}}{a factor with levels \code{black} \code{hispanic} \code{white}}
    \item{\code{heroin}}{a numeric vector}
    \item{\code{crack}}{a numeric vector}
    \item{\code{cocaine}}{a numeric vector}
    \item{\code{marijuan}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Inmate)
attach(Inmate)
Inmate
chisq.test(Inmate[,2:5])
detach(Inmate)
}
\keyword{datasets}
