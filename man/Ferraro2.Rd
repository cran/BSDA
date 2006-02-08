\name{Ferraro2}
\alias{Ferraro2}
\docType{data}
\title{Choice of vice presidental candidate in 1984 by gender}
\description{
  Data for Exercise 8.23
}
\usage{Ferraro2}
\format{
  A data frame with 2 observations on the following 4 variables.
  \describe{
    \item{\code{gender}}{a factor with levels \code{Men} \code{Women}}
    \item{\code{Bush}}{a numeric vector}
    \item{\code{Ferraro}}{a numeric vector}
    \item{\code{undecide}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Ferraro2)
attach(Ferraro2)
Ferraro2
chisq.test(Ferraro2[,2:4])
detach(Ferraro2)
}
\keyword{datasets}
