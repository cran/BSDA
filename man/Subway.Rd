\name{Subway}
\alias{Subway}
\docType{data}
\title{ Time it takes a subway to travel from the airport to downtown}
\description{
  Data for Exercise 5.19
}
\usage{Subway}
\format{
  A data frame with 30 observations on the following variable.
  \describe{
    \item{\code{time}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Subway)
attach(Subway)
EDA(time)
detach(Subway)
}
\keyword{datasets}
