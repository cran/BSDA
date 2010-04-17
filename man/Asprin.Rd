\name{Asprin}
\alias{Asprin}
\docType{data}
\title{Dissolving time versus level of impurities in aspirin tablets}
\description{
  Data for Exercise 10.51 
}
\usage{Asprin}
\format{
  A data frame with 5 observations on the following 3 variables.
  \describe{
    \item{\code{X1.}}{a numeric vector}
    \item{\code{X5.}}{a numeric vector}
    \item{\code{X10.}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Asprin)
attach(Asprin)
STACKED <-stack(Asprin)
STACKED[1:5,]
boxplot(values~ind,col=c("red","blue","green"),data=STACKED)
anova(lm(values~ind,data=STACKED))
remove(STACKED)
detach(Asprin)
}
\keyword{datasets}
