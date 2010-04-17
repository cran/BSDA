\name{Cat}
\alias{Cat}
\docType{data}
\title{Reading scores on the California achievement test for a group of 3rd graders}
\description{
  Data for Exercise 1.116
}
\usage{Cat}
\format{
  A data frame with 17 observations on the following variable.
  \describe{
    \item{\code{score}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Cat)
attach(Cat)
stem(score)
fivenum(score)
boxplot(score,main="Problem 1.116",col="green")
detach(Cat)
}
\keyword{datasets}
