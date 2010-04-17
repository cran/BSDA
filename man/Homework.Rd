\name{Homework}
\alias{Homework}
\docType{data}
\title{Number of hours per week spent on homework for private and public high school students}
\description{
  Data for Exercise 7.78
}
\usage{Homework}
\format{
  A data frame with 15 observations on the following 2 variables.
  \describe{
    \item{\code{Private}}{a numeric vector}
    \item{\code{Public}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Homework)
attach(Homework)
boxplot(Private,Public)
t.test(Private,Public,conf.level=.98)
detach(Homework)
}
\keyword{datasets}
