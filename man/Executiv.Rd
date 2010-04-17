\name{Executiv}
\alias{Executiv}
\docType{data}
\title{Ages of 25 executives}
\description{
  Data for Exercise 1.104
}
\usage{Executiv}
\format{
  A data frame with 25 observations on the following variable.
  \describe{
    \item{\code{Age}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Executiv)
attach(Executiv)
EDA(Age)
detach(Executiv)
}
\keyword{datasets}
