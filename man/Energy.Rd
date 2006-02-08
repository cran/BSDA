\name{Energy}
\alias{Energy}
\docType{data}
\title{Amount of energy consumed by homes versus their sizes}
\description{
  Data for Exercises 2.5, 2.24, and 2.55
}
\usage{Energy}
\format{
  A data frame with 12 observations on the following 5 variables.
  \describe{
    \item{\code{Size}}{a numeric vector}
    \item{\code{kilowatt}}{a numeric vector}
    \item{\code{SRES1}}{a numeric vector}
    \item{\code{FITS1}}{a numeric vector}
    \item{\code{Residuals}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Energy)
attach(Energy)
plot(Size,kilowatt)
cor(Size,kilowatt)
model <- lm(kilowatt~Size)
plot(Size,resid(model))
detach(Energy)
}
\keyword{datasets}
