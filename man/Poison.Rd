\name{Poison}
\alias{Poison}
\docType{data}
\title{Number of poisonings reported to 16 poison control centers}
\description{
  Data for Exercise 1.113
}
\usage{Poison}
\format{
  A data frame with 6 observations on the following 2 variables.
  \describe{
    \item{\code{Type}}{a factor with levels \code{Alcohol} \code{Cleaning agent} \code{Cosmetics} \code{Drugs} \code{Insecticides} \code{Plants}}
    \item{\code{number}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Poison)
attach(Poison)
names(number) <- Type
barplot(number,col="red")
}
\keyword{datasets}
