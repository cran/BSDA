\name{Irises}
\alias{Irises}
\docType{data}
\title{R.A. Fishers famous data on sepal length of a species of Iris Setosa}
\description{
  Data for Exercises 1.15 and 5.19
}
\usage{Irises}
\format{
  A data frame with 150 observations on the following 14 variables.
  \describe{
    \item{\code{sepalL1}}{a numeric vector}
    \item{\code{sepalW1}}{a numeric vector}
    \item{\code{petalL1}}{a numeric vector}
    \item{\code{petalW1}}{a numeric vector}
    \item{\code{sepalL2}}{a numeric vector}
    \item{\code{sepalW2}}{a numeric vector}
    \item{\code{petalL2}}{a numeric vector}
    \item{\code{peatalW2}}{a numeric vector}
    \item{\code{sepalL3}}{a numeric vector}
    \item{\code{sepalW3}}{a numeric vector}
    \item{\code{petalL3}}{a numeric vector}
    \item{\code{petalW3}}{a numeric vector}
    \item{\code{sepalL}}{a numeric vector}
    \item{\code{sample}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Irises)
attach(Irises)
EDA(sepalL1)
t.test(sepalL1,conf.level=.99)$conf
detach(Irises)
}
\keyword{datasets}
