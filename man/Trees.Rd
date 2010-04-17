\name{Trees}
\alias{Trees}
\docType{data}
\title{Number of trees in 20 grids}
\description{
  Data for Exercise 1.50
}
\usage{Trees}
\format{
  A data frame with 20 observations on the following variable.
  \describe{
    \item{\code{number}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Trees)
attach(Trees)
stem(number)
hist(number,breaks=seq(60,110,10),right=FALSE,col="green",main="Problem 1.50")
detach(Trees)
}
\keyword{datasets}
