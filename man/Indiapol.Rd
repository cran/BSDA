\name{Indiapol}
\alias{Indiapol}
\docType{data}
\title{Average miles per hour for the winners of the Indianapolis 500 race}
\description{
  Data for Exercise 1.128
}
\usage{Indiapol}
\format{
  A data frame with 39 observations on the following 3 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{speed}}{a numeric vector}
    \item{\code{yr.1960}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Indiapol)
attach(Indiapol)
plot(year,speed,type="l")
detach(Indiapol)
}
\keyword{datasets}
