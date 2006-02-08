\name{Devmath}
\alias{Devmath}
\docType{data}
\title{Test scores for students who failed developmental mathematics in the fall semester 1995}
\description{
  Data for Exercise 6.47
}
\usage{Devmath}
\format{
  A data frame with 40 observations on the following variable.
  \describe{
    \item{\code{score}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Devmath)
attach(Devmath)
EDA(score)
t.test(score,mu=80,alternative="less")
detach(Devmath)
}
\keyword{datasets}
