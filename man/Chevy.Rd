\name{Chevy}
\alias{Chevy}
\docType{data}
\title{Insurance injury ratings of Chevrolet vehicles for 1990 and 1993 models}
\description{
  Data for Exercise 8.35
}
\usage{data(Chevy)}
\format{
  A data frame with 2 observations on the following 6 variables.
  \describe{
    \item{\code{year}}{a factor with levels \code{88-90} \code{91-93}}
    \item{\code{A}}{a numeric vector}
    \item{\code{B}}{a numeric vector}
    \item{\code{C}}{a numeric vector}
    \item{\code{D}}{a numeric vector}
    \item{\code{F}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Chevy)
attach(Chevy)
Chevy
chisq.test(Chevy[,2:6])
detach(Chevy)
}
\keyword{datasets}
