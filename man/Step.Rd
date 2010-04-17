\name{Step}
\alias{Step}
\docType{data}
\title{ STEP science test scores for a class of ability-grouped students}
\description{
  Data for Exercise 6.79
}
\usage{Step}
\format{
  A data frame with 12 observations on the following variable.
  \describe{
    \item{\code{score}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Step)
attach(Step)
EDA(score)
t.test(score,mu=80,alternative="less")
detach(Step)
}
\keyword{datasets}
