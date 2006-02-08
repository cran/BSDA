\name{Alcohol}
\alias{Alcohol}
\docType{data}

\title{Ages at which 14 female alcoholics began drinking}

\description{Data used in Exercise 5.79}

\usage{Alcohol}

\format{
  A data frame with 14 observations on the following variable.
  \describe{
    \item{\code{age}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
attach(Alcohol)
qqnorm(age)
qqline(age)
sign.test(age,md=20,conf.level=0.99)
detach(Alcohol)
}

\keyword{datasets}
