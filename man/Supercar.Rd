\name{Supercar}
\alias{Supercar}
\docType{data}
\title{Top speeds attained by five makes of supercars}
\description{
  Data for Statistical Insight Chapter 10
}
\usage{Supercar}
\format{
  A data frame with 30 observations on the following 7 variables.
  \describe{
    \item{\code{Acura}}{a numeric vector}
    \item{\code{Ferrari}}{a numeric vector}
    \item{\code{Lotus}}{a numeric vector}
    \item{\code{Porsche}}{a numeric vector}
    \item{\code{Viper}}{a numeric vector}
    \item{\code{speed}}{a numeric vector}
    \item{\code{car}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Supercar)
attach(Supercar)
boxplot(speed~car)
anova(lm(speed~as.factor(car)))
detach(Supercar)
}
\keyword{datasets}
