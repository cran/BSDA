\name{Earthqk}
\alias{Earthqk}
\docType{data}
\title{One hundred year record of worldwide seismic activity(1770-1869)}
\description{
  Data for Exercise 6.97
}
\usage{Earthqk}
\format{
  A data frame with 100 observations on the following 2 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{severity}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Earthqk)
attach(Earthqk)
EDA(severity)
t.test(severity,mu=100,alternative="greater")
detach(Earthqk)
}
\keyword{datasets}
