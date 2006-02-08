\name{Cigar}
\alias{Cigar}
\docType{data}
\title{Milligrams of tar in 25 cigarettes selected randomly from 4 different brands}
\description{
  Data for Example 10.4 
}
\usage{Cigar}
\format{
  A data frame with 100 observations on the following 6 variables.
  \describe{
    \item{\code{brandA}}{a numeric vector}
    \item{\code{brandB}}{a numeric vector}
    \item{\code{brandC}}{a numeric vector}
    \item{\code{brandD}}{a numeric vector}
    \item{\code{tar}}{a numeric vector}
    \item{\code{brand}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Cigar)
attach(Cigar)
boxplot(tar~brand)
anova(lm(tar~as.factor(brand)))
detach(Cigar)
}
\keyword{datasets}
