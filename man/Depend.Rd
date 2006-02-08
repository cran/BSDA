\name{Depend}
\alias{Depend}
\docType{data}
\title{Number of dependent children for 50 families}
\description{
  Data for Exercise 1.26
}
\usage{Depend}
\format{
  A data frame with 50 observations on the following 4 variables.
  \describe{
    \item{\code{C1}}{a numeric vector}
    \item{\code{number}}{a numeric vector}
    \item{\code{Count}}{a numeric vector}
    \item{\code{Percent}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Depend)
attach(Depend)
table(C1)
barplot(table(C1),col="lightblue",main="Problem 1.26",
xlab="Number of Dependent Children",ylab="Frequency")
detach(Depend)
}
\keyword{datasets}
