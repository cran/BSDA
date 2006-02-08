\name{Archaeo}
\alias{Archaeo}
\docType{data}
\title{Radiocarbon ages of observations taken from an archaeological site}
\description{
  Data for Exercises 5.120, 10.20 and Example 1.16 
}
\usage{Archaeo}
\format{
  A data frame with 60 observations on the following 6 variables.
  \describe{
    \item{\code{phase1}}{a numeric vector}
    \item{\code{phase2}}{a numeric vector}
    \item{\code{phase3}}{a numeric vector}
    \item{\code{phase4}}{a numeric vector}
    \item{\code{age}}{a numeric vector}
    \item{\code{phase}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Archaeo)
attach(Archaeo)
boxplot(age~phase,col="yellow",main="Example 1.16",xlab="Phase",ylab="Age")
anova(lm(age~as.factor(phase)))
detach(Archaeo)
}
\keyword{datasets}
