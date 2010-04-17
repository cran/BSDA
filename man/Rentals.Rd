\name{Rentals}
\alias{Rentals}
\docType{data}
\title{Weekly rentals for 45 apartments}
\description{
  Data for Exercise 1.122
}
\usage{Rentals}
\format{
  A data frame with 45 observations on the following variable.
  \describe{
    \item{\code{rent}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Rentals)
attach(Rentals)
EDA(rent)
detach(Rentals)
}
\keyword{datasets}
