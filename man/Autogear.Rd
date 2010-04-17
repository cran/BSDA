\name{Autogear}
\alias{Autogear}
\docType{data}
\title{Number of defective auto gears produced by two manufacturers}
\description{
  Data for Exercise 7.46
}
\usage{Autogear}
\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{A}}{a numeric vector}
    \item{\code{B}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Autogear)
attach(Autogear)
t.test(A,B)
wilcox.test(A,B)
t.test(A,B,var.equal=TRUE)
detach(Autogear)
}
\keyword{datasets}
