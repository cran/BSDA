\name{Psat}
\alias{Psat}
\docType{data}
\title{PSAT scores versus SAT scores}
\description{
  Data for Exercise 2.50
}
\usage{Psat}
\format{
  A data frame with 7 observations on the following 4 variables.
  \describe{
    \item{\code{psat}}{a numeric vector}
    \item{\code{sat}}{a numeric vector}
    \item{\code{SRES1}}{a numeric vector}
    \item{\code{FITS1}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Psat)
attach(Psat)
model <- lm(sat~psat)
plot(psat,resid(model))
detach(Psat)
}
\keyword{datasets}
