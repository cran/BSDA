\name{Saving}
\alias{Saving}
\docType{data}
\title{Problem asset ration for savings and loan companies in California, New York, and Texas}
\description{
  Data for Exercise 10.34 and 10.49
}
\usage{Saving}
\format{
  A data frame with 75 observations on the following 6 variables.
  \describe{
    \item{\code{calif}}{a numeric vector}
    \item{\code{newyork}}{a numeric vector}
    \item{\code{texas}}{a numeric vector}
    \item{\code{PAR}}{a numeric vector}
    \item{\code{state}}{a numeric vector}
    \item{\code{ranks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Saving)
attach(Saving)
boxplot(PAR~state)
kruskal.test(PAR~as.factor(state))
detach(Saving)
}
\keyword{datasets}
