\name{Drug}
\alias{Drug}
\docType{data}
\title{Number of trials to master a task for a group of 28 subjects assigned to a control and an experimental group}
\description{
  Data for Exercise 7.15
}
\usage{Drug}
\format{
  A data frame with 28 observations on the following 2 variables.
  \describe{
    \item{\code{trials}}{a numeric vector}
    \item{\code{group}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Drug)
attach(Drug)
boxplot(trials~group)
wilcox.test(trials~group)
detach(Drug)
}
\keyword{datasets}
