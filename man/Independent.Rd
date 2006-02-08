\name{Independent}
\alias{Independent}
\docType{data}
\title{Illustrates a comparison problem for long-tailed distributions}
\description{
  Data for Exercise 7.41
}
\usage{Independent}
\format{
  A data frame with 46 observations on the following 3 variables.
  \describe{
    \item{\code{score}}{a numeric vector}
    \item{\code{group}}{a numeric vector}
    \item{\code{ranks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Independent)
attach(Independent)
boxplot(score~group)
wilcox.test(score~group)
detach(Independent)
}
\keyword{datasets}
