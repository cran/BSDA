\name{Bones}
\alias{Bones}
\docType{data}
\title{Bone density measurements of 35 physically active and 35 non-active women}
\description{
  Data for Exercise 7.22 
}
\usage{Bones}
\format{
  A data frame with 70 observations on the following 5 variables.
  \describe{
    \item{\code{Active}}{a numeric vector}
    \item{\code{Nonactive}}{a numeric vector}
    \item{\code{Density}}{a numeric vector}
    \item{\code{group}}{a numeric vector}
    \item{\code{Ranks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Bones)
attach(Bones)
t.test(Active,Nonactive,alternative="greater")
wilcox.test(Active,Nonactive,alternative="greater")
detach(Bones)
}
\keyword{datasets}
