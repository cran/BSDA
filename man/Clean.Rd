\name{Clean}
\alias{Clean}
\docType{data}
\title{Residual contaminant following the use of three different cleansing agents}
\description{
  Data for Exercise 10.16
}
\usage{Clean}
\format{
  A data frame with 45 observations on the following 5 variables.
  \describe{
    \item{\code{A}}{a numeric vector}
    \item{\code{B}}{a numeric vector}
    \item{\code{C}}{a numeric vector}
    \item{\code{clean}}{a numeric vector}
    \item{\code{agent}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Clean)
attach(Clean)
boxplot(clean~agent,col=c("red","blue","green"))
anova(lm(clean~as.factor(agent)))
detach(Clean)
}
\keyword{datasets}
