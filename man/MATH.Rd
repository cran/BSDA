\name{MATH}
\alias{MATH}
\docType{data}
\title{Standardized math test scores for 30 students}
\description{
  Data for Exercise 1.69
}
\usage{MATH}
\format{
  A data frame with 30 observations on the following variable.
  \describe{
    \item{\code{math}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(MATH)
attach(MATH)
hist(math,col="pink")
CharlieZ <- (62-mean(math))/sd(math)
CharlieZ
detach(MATH)
remove(CharlieZ)
}
\keyword{datasets}
