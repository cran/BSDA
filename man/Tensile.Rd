\name{Tensile}
\alias{Tensile}
\docType{data}
\title{ Tensile strength of plastic bags from two production runs}
\description{
  Data for Example 7.11
}
\usage{Tensile}
\format{
  A data frame with 72 observations on the following 4 variables.
  \describe{
    \item{\code{Run.1}}{a numeric vector}
    \item{\code{Run.2}}{a numeric vector}
    \item{\code{Tensile}}{a numeric vector}
    \item{\code{Run}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Tensile)
attach(Tensile)
boxplot(Run.1,Run.2,names=c("Run 1","Run 2"),col=c("red","Blue"))
boxplot(Tensile~Run,names=c("Run 1","Run 2"),col=c("red","Blue"))
t.test(Tensile~Run)
t.test(Run.1,Run.2)
detach(Tensile)
}
\keyword{datasets}
