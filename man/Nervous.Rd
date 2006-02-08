\name{Nervous}
\alias{Nervous}
\docType{data}
\title{Reaction effects of 4 drugs on 25 subjects with a nervous disorder}
\description{
  Data for Exercise 10.3
}
\usage{Nervous}
\format{
  A data frame with 25 observations on the following 2 variables.
  \describe{
    \item{\code{react}}{a numeric vector}
    \item{\code{drug}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Nervous)
attach(Nervous)
boxplot(react~drug)
anova(lm(react~as.factor(drug)))
detach(Nervous)
}
\keyword{datasets}
