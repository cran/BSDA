\name{Tenness}
\alias{Tenness}
\docType{data}
\title{Tennessee self concept scores for 20 gifted high school students}
\description{
  Data for Exercise 6.56
}
\usage{Tenness}
\format{
  A data frame with 20 observations on the following variable.
  \describe{
    \item{\code{score}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Tenness)
attach(Tenness)
EDA(score)
t.test(score,mu=30,alternative="less")
SIGN.test(score,md=30,alternative="less")
detach(Tenness)
}
\keyword{datasets}
