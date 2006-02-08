\name{Board}
\alias{Board}
\docType{data}
\title{Incomes of board members from three different universities}
\description{
  Data for Exercise 10.14
}
\usage{Board}
\format{
  A data frame with 7 observations on the following 3 variables.
  \describe{
    \item{\code{UnivA}}{a numeric vector}
    \item{\code{UnivB}}{a numeric vector}
    \item{\code{UnivC}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Board)
attach(Board)
STACKED <-stack(Board)
STACKED[1:5,]
boxplot(values~ind,col=c("red","blue","green"),data=STACKED)
remove(STACKED)
detach(Board)
}
\keyword{datasets}
