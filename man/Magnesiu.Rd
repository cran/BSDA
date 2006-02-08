\name{Magnesiu}
\alias{Magnesiu}
\docType{data}
\title{Magnesium concentration and distances between samples}
\description{
  Data for Exercise 9.9
}
\usage{Magnesiu}
\format{
  A data frame with 20 observations on the following 4 variables.
  \describe{
    \item{\code{distance}}{a numeric vector}
    \item{\code{magnesiu}}{a numeric vector}
    \item{\code{SRES1}}{a numeric vector}
    \item{\code{FITS1}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Magnesiu)
attach(Magnesiu)
model <- lm(magnesiu~distance)
plot(distance,magnesiu)
abline(model)
summary(model)
detach(Magnesiu)
remove(model)
}
\keyword{datasets}
