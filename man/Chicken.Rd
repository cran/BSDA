\name{Chicken}
\alias{Chicken}
\docType{data}
\title{Weight gain of chickens fed three different rations}
\description{
  Data for Exercise 10.15
}
\usage{data(Chicken)}
\format{
  A data frame with 13 observations on the following 3 variables.
  \describe{
    \item{\code{Ration1}}{a numeric vector}
    \item{\code{Ration2}}{a numeric vector}
    \item{\code{Ration3}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Chicken)
attach(Chicken)
STACKED <-stack(Chicken)
STACKED[1:5,]
boxplot(values~ind,col=c("red","blue","green"),data=STACKED)
anova(lm(values~ind,data=STACKED))
remove(STACKED)
detach(Chicken)
}
\keyword{datasets}
