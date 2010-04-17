\name{Dice}
\alias{Dice}
\docType{data}
\title{Outcomes and probabilities of the roll of a pair of fair dice}
\description{
  Data for Exercise 3.109
}
\usage{Dice}
\format{
  A data frame with 11 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{P.x.}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Dice)
attach(Dice)
roll1 <- sample(1:6,2000,replace=TRUE)
roll2 <- sample(1:6,2000,replace=TRUE)
outcome <- roll1+roll2
table(outcome)/length(outcome)
detach(Dice)
remove(roll1,roll2,outcome)
}
\keyword{datasets}
