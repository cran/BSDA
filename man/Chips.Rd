\name{Chips}
\alias{Chips}
\docType{data}
\title{Four measurements on a first wafer and four measurements on a second wafer selected from 30 lots}
\description{
  Data for Exercise 10.9
}
\usage{Chips}
\format{
  A data frame with 30 observations on the following 8 variables.
  \describe{
    \item{\code{wafer11}}{a numeric vector}
    \item{\code{wafer12}}{a numeric vector}
    \item{\code{wafer13}}{a numeric vector}
    \item{\code{wafer14}}{a numeric vector}
    \item{\code{wafer21}}{a numeric vector}
    \item{\code{wafer22}}{a numeric vector}
    \item{\code{wafer23}}{a numeric vector}
    \item{\code{wafer24}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Chips)
attach(Chips)
boxplot(wafer11,wafer12,wafer13,wafer14,wafer21,wafer22,wafer23,wafer24)
detach(Chips)
}
\keyword{datasets}
