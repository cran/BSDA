\name{Spellers}
\alias{Spellers}
\docType{data}
\title{ Standardized spelling test scores for two fourth grade classes}
\description{
  Data for Exercise 7.82
}
\usage{Spellers}
\format{
  A data frame with 10 observations on the following 2 variables.
  \describe{
    \item{\code{Fourth}}{a numeric vector}
    \item{\code{Colleag}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Spellers)
attach(Spellers)
t.test(Fourth,Colleag,alternative="greater")
detach(Spellers)
}
\keyword{datasets}
