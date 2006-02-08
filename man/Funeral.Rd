\name{Funeral}
\alias{Funeral}
\docType{data}
\title{Cost of funeral by region of country}
\description{
  Data for Exercise 8.54
}
\usage{Funeral}
\format{
  A data frame with 4 observations on the following 4 variables.
  \describe{
    \item{\code{Region}}{a factor with levels \code{Central} \code{East} \code{South} \code{West}}
    \item{\code{Less}}{a numeric vector}
    \item{\code{Average}}{a numeric vector}
    \item{\code{More}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Funeral)
attach(Funeral)
Funeral
chisq.test(Funeral[,2:4])
detach(Funeral)
}
\keyword{datasets}
