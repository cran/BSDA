\name{Eggs}
\alias{Eggs}
\docType{data}
\title{Number of eggs versus amounts of feed supplement}
\description{
  Data for Exercise 9.22
}
\usage{Eggs}
\format{
  A data frame with 12 observations on the following 7 variables.
  \describe{
    \item{\code{feed}}{a numeric vector}
    \item{\code{eggs}}{a numeric vector}
    \item{\code{SRES1}}{a numeric vector}
    \item{\code{FITS1}}{a numeric vector}
    \item{\code{c1sq}}{a numeric vector}
    \item{\code{SRES2}}{a numeric vector}
    \item{\code{FITS2}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Eggs)
attach(Eggs)
plot(feed,eggs)
model <- lm(eggs~feed)
abline(model)
summary(model)
detach(Eggs)
remove(model)
}
\keyword{datasets}
