\name{Monoxide}
\alias{Monoxide}
\docType{data}
\title{Carbon monoxide emitted by smoke stacks of a manufacturer and a competitor}
\description{
  Data for Exercise 7.45
}
\usage{Monoxide}
\format{
  A data frame with 10 observations on the following 2 variables.
  \describe{
    \item{\code{manufac}}{a numeric vector}
    \item{\code{compet}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Monoxide)
attach(Monoxide)
t.test(manufac,compet)
wilcox.test(manufac,compet)
detach(Monoxide)
}
\keyword{datasets}
