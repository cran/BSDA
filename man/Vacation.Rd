\name{Vacation}
\alias{Vacation}
\docType{data}
\title{Number of days of paid holidays and vacation leave for sample of 35 textile workers}
\description{
  Data for Exercise 6.46 and 6.98
}
\usage{Vacation}
\format{
  A data frame with 35 observations on the following variable.
  \describe{
    \item{\code{number}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Vacation)
attach(Vacation)
EDA(number)
t.test(number,mu=24)
detach(Vacation)
}
\keyword{datasets}
