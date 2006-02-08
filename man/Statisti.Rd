\name{Statisti}
\alias{Statisti}
\docType{data}
\title{Test scores for two beginning statistics classes}
\description{
  Data for Exercises 1.70 and 1.87
}
\usage{Statisti}
\format{
  A data frame with 31 observations on the following 2 variables.
  \describe{
    \item{\code{Class1}}{a numeric vector}
    \item{\code{Class2}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Statisti)
attach(Statisti)
boxplot(Class1,Class2,names=c("Class 1","Class 2"),col=c("red","blue"),
main="Problem 1.87")
detach(Statisti)
}
\keyword{datasets}
