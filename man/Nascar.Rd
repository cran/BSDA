\name{Nascar}
\alias{Nascar}
\docType{data}
\title{Efficiency of pit crews for three major NASCAR teams}
\description{
  Data for Example 10.53
}
\usage{Nascar}
\format{
  A data frame with 36 observations on the following 6 variables.
  \describe{
    \item{\code{TeamA}}{a numeric vector}
    \item{\code{TeamB}}{a numeric vector}
    \item{\code{TeamC}}{a numeric vector}
    \item{\code{Time}}{a numeric vector}
    \item{\code{Team}}{a numeric vector}
    \item{\code{Ranks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Nascar)
attach(Nascar)
boxplot(Time~Team)
anova(lm(Time~as.factor(Team)))
detach(Nascar)
}
\keyword{datasets}
