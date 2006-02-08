\name{Seatbelt}
\alias{Seatbelt}
\docType{data}
\title{Injury level versus seatbelt usage}
\description{
  Data for Exercise 8.24
}
\usage{Seatbelt}
\format{
  A data frame with 2 observations on the following 5 variables.
  \describe{
    \item{\code{seatbelt}}{a factor with levels \code{no} \code{yes}}
    \item{\code{None}}{a numeric vector}
    \item{\code{Minimal}}{a numeric vector}
    \item{\code{Minor}}{a numeric vector}
    \item{\code{Major}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Seatbelt)
attach(Seatbelt)
Seatbelt
chisq.test(Seatbelt[,2:5])
detach(Seatbelt)
}
\keyword{datasets}
