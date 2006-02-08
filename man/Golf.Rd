\name{Golf}
\alias{Golf}
\docType{data}
\title{Distances a golf ball can be driven by 20 professional golfers}
\description{
  Data for Exercise 5.24
}
\usage{Golf}
\format{
  A data frame with 20 observations on the following variable.
  \describe{
    \item{\code{yards}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Golf)
attach(Golf)
stem(yards)
EDA(yards)
detach(Golf)
}
\keyword{datasets}
