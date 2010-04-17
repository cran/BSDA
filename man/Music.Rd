\name{Music}
\alias{Music}
\docType{data}
\title{Improvement scores for identical twins taught music recognition by two techniques }
\description{
  Data for Exercise 7.59
}
\usage{Music}
\format{
  A data frame with 12 observations on the following 3 variables.
  \describe{
    \item{\code{Method1}}{a numeric vector}
    \item{\code{Method2}}{a numeric vector}
    \item{\code{differ}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Music)
attach(Music)
qqnorm(differ)
qqline(differ)
shapiro.test(differ)
t.test(Method1,Method2,paired=TRUE)
detach(Music)
}
\keyword{datasets}
