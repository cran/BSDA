\name{Backtoback}
\alias{Backtoback}
\docType{data}
\title{Illustrates inferences based on pooled t-test versus Wilcoxon rank sum test}
\description{
  Data for Exercise 7.40
}
\usage{Backtoback}
\format{
  A data frame with 24 observations on the following 3 variables.
  \describe{
    \item{\code{score}}{a numeric vector}
    \item{\code{group}}{a numeric vector}
    \item{\code{ranks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Backtoback)
attach(Backtoback)
wilcox.test(score~group)
detach(Backtoback)
}
\keyword{datasets}
