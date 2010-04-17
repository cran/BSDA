\name{Ferraro1}
\alias{Ferraro1}
\docType{data}
\title{Choice of presidental ticket in 1984 by gender}
\description{
  Data for Exercise 8.23
}
\usage{Ferraro1}
\format{
  A data frame with 2 observations on the following 4 variables.
  \describe{
    \item{\code{gender}}{a factor with levels \code{Men} \code{Women}}
    \item{\code{Reag.Bs}}{a numeric vector}
    \item{\code{Mond.Fer}}{a numeric vector}
    \item{\code{undecide}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Ferraro1)
attach(Ferraro1)
Ferraro1
chisq.test(Ferraro1[,2:4])
detach(Ferraro1)
}
\keyword{datasets}
