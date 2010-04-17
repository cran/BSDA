\name{Insulate}
\alias{Insulate}
\docType{data}
\title{Heat loss through a new insulating medium}
\description{
  Data for Exercise 9.50
}
\usage{Insulate}
\format{
  A data frame with 10 observations on the following 2 variables.
  \describe{
    \item{\code{temp}}{a numeric vector}
    \item{\code{loss}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Insulate)
attach(Insulate)
summary(lm(loss~temp))
detach(Insulate)
}
\keyword{datasets}
