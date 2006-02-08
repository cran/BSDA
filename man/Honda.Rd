\name{Honda}
\alias{Honda}
\docType{data}
\title{Miles per gallon for a Honda Civic on 35 different occasions}
\description{
  Data for Statistical Insight Chapter 6
}
\usage{Honda}
\format{
  A data frame with 35 observations on the following variable.
  \describe{
    \item{\code{mileage}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Honda)
attach(Honda)
t.test(mileage,mu=40,alternative="less")
detach(Honda)
}
\keyword{datasets}
