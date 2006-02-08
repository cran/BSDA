\name{Twin}
\alias{Twin}
\docType{data}
\title{Intelligence test scores for identical twins in which one twin is given a drug}
\description{
  Data for Exercise 7.54
}
\usage{Twin}
\format{
  A data frame with 9 observations on the following 3 variables.
  \describe{
    \item{\code{TwinA}}{a numeric vector}
    \item{\code{TwinB}}{a numeric vector}
    \item{\code{differ}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Twin)
attach(Twin)
qqnorm(differ)
qqline(differ)
shapiro.test(differ)
t.test(TwinA,TwinB,paired=TRUE)
detach(Twin)
}
\keyword{datasets}
