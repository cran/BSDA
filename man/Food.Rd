\name{Food}
\alias{Food}
\docType{data}
\title{Annual food expenditures for 40 single households in Ohio}
\description{
  Data for Exercise 5.106
}
\usage{Food}
\format{
  A data frame with 40 observations on the following variable.
  \describe{
    \item{\code{food}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Food)
attach(Food)
EDA(food)
detach(Food)
}
\keyword{datasets}
