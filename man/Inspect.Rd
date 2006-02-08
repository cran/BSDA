\name{Inspect}
\alias{Inspect}
\docType{data}
\title{Percent of vehicles passing inspection by type inspection station}
\description{
  Data for Exercise 5.89
}
\usage{Inspect}
\format{
  A data frame with 6 observations on the following 4 variables.
  \describe{
    \item{\code{Type}}{a factor with levels \code{auto inspection} \code{auto repair} \code{car care center} \code{gas station} \code{new car dealer} \code{tire store}}
    \item{\code{less70}}{a numeric vector}
    \item{\code{X70.85}}{a numeric vector}
    \item{\code{great85}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Inspect)
attach(Inspect)
Inspect
chisq.test(Inspect[,2:4])
detach(Inspect)
}
\keyword{datasets}
