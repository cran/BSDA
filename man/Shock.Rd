\name{Shock}
\alias{Shock}
\docType{data}
\title{Effect of experimental shock on time to complete difficult task}
\description{
  Data for Exercise 10.50
}
\usage{Shock}
\format{
  A data frame with 9 observations on the following 3 variables.
  \describe{
    \item{\code{Group1}}{a numeric vector}
    \item{\code{Group2}}{a numeric vector}
    \item{\code{Group3}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Shock)
attach(Shock)
STACKED <-stack(Shock)
STACKED[1:5,]
boxplot(values~ind,col=c("red","blue","green"),data=STACKED)
anova(lm(values~ind,data=STACKED))
remove(STACKED)
detach(Shock)

}
\keyword{datasets}
