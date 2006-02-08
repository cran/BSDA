\name{Laminect}
\alias{Laminect}
\docType{data}
\title{ Median costs of laminectomies at hospitals across North Carolina in 1992}
\description{
  Data for Exercise 10.18
}
\usage{Laminect}
\format{
  A data frame with 46 observations on the following 5 variables.
  \describe{
    \item{\code{cost}}{a numeric vector}
    \item{\code{class}}{a numeric vector}
    \item{\code{Rural}}{a numeric vector}
    \item{\code{Regional}}{a numeric vector}
    \item{\code{Metropol}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Laminect)
attach(Laminect)
boxplot(cost~class)
anova(lm(cost~as.factor(class)))
detach(Laminect)
}
\keyword{datasets}
