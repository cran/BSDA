\name{Appendec}
\alias{Appendec}
\docType{data}
\title{ Median costs of appendectomies at three different types of North Carolina hospitals}
\description{
  Data for Exercise 10.60 
}
\usage{Appendec}
\format{
  A data frame with 59 observations on the following 5 variables.
  \describe{
    \item{\code{Cost}}{a numeric vector}
    \item{\code{Region}}{a numeric vector}
    \item{\code{Rural}}{a numeric vector}
    \item{\code{Regional}}{a numeric vector}
    \item{\code{Metropol}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Appendec)
attach(Appendec)
boxplot(Cost~Region)
anova(lm(Cost~as.factor(Region)))
detach(Appendec)
}
\keyword{datasets}
