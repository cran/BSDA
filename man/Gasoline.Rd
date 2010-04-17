\name{Gasoline}
\alias{Gasoline}
\docType{data}
\title{Price of regular unleaded gasoline obtained from 25 service stations}
\description{
  Data for Exercise 1.45
}
\usage{Gasoline}
\format{
  A data frame with 25 observations on the following variable.
  \describe{
    \item{\code{price}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Gasoline)
attach(Gasoline)
stem(price)
detach(Gasoline)
}
\keyword{datasets}
