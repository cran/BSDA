\name{Ability}
\alias{Ability}
\docType{data}

\title{Perceived math ability for 13-year olds by gender}

\description{Data used in Exercise 8.57}

\usage{Ability}

\format{
  A data frame with 2 observations on the following 6 variables.
  \describe{
    \item{\code{gender}}{a factor with levels \code{boys} \code{girls}}
    \item{\code{hopeless}}{a numeric vector}
    \item{\code{belowavg}}{a numeric vector}
    \item{\code{average}}{a numeric vector}
    \item{\code{aboveavg}}{a numeric vector}
    \item{\code{superior}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
X <- as.matrix(Ability[1:2,2:6])
chisq.test(X)
}
\keyword{datasets}
