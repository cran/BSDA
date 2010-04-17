\name{Skewed}
\alias{Skewed}
\docType{data}
\title{ Illustrates the Wilcoxon Rank Sum test}
\description{
  Data for Exercise 7.65
}
\usage{Skewed}
\format{
  A data frame with 21 observations on the following 2 variables.
  \describe{
    \item{\code{C1}}{a numeric vector}
    \item{\code{C2}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Skewed)
attach(Skewed)
boxplot(C1,C2)
wilcox.test(C1,C2)
detach(Skewed)
}
\keyword{datasets}
