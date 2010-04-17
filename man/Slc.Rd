\name{Slc}
\alias{Slc}
\docType{data}
\title{Sodium-lithium countertransport activity on 190 individuals from six large English kindred}
\description{
  Data for Exercise 5.116
}
\usage{Slc}
\format{
  A data frame with 190 observations on the following variable.
  \describe{
    \item{\code{SLC}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Slc)
attach(Slc)
EDA(SLC)
detach(Slc)
}
\keyword{datasets}
