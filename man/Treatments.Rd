\name{Treatments}
\alias{Treatments}
\docType{data}
\title{Illustrates analysis of variance for three treatment groups}
\description{
  Data for Exercise 10.44
}
\usage{Treatments}
\format{
  A data frame with 24 observations on the following 5 variables.
  \describe{
    \item{\code{treat1}}{a numeric vector}
    \item{\code{treat2}}{a numeric vector}
    \item{\code{treat3}}{a numeric vector}
    \item{\code{Treatmnt}}{a numeric vector}
    \item{\code{Group}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Treatments)
attach(Treatments)
anova(lm(Treatmnt~as.factor(Group)))
detach(Treatments)
}
\keyword{datasets}
