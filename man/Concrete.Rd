\name{Concrete}
\alias{Concrete}
\docType{data}
\title{Compressive strength of concrete blocks made by two different methods}
\description{
  Data for Exercise 7.17
}
\usage{Concrete}
\format{
  A data frame with 20 observations on the following 3 variables.
  \describe{
    \item{\code{Strength}}{a numeric vector}
    \item{\code{Method}}{a numeric vector}
    \item{\code{Ranks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Concrete)
attach(Concrete)
wilcox.test(Strength~Method,alternative="greater")
detach(Concrete)
}
\keyword{datasets}
