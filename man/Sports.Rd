\name{Sports}
\alias{Sports}
\docType{data}
\title{ Favorite sport by gender}
\description{
  Data for Exercise 8.32
}
\usage{Sports}
\format{
  A data frame with 2 observations on the following 4 variables.
  \describe{
    \item{\code{Football}}{a numeric vector}
    \item{\code{Basketball}}{a numeric vector}
    \item{\code{Baseball}}{a numeric vector}
    \item{\code{Tennis}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Sports)
attach(Sports)
chisq.test(Sports)
detach(Sports)
}
\keyword{datasets}
