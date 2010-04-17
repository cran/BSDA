\name{Groups}
\alias{Groups}
\docType{data}
\title{An illustration of analysis of variance}
\description{
  Data for Exercise 10.4
}
\usage{Groups}
\format{
  A data frame with 26 observations on the following 3 variables.
  \describe{
    \item{\code{GroupA}}{a numeric vector}
    \item{\code{GroupB}}{a numeric vector}
    \item{\code{GroupC}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Groups)
attach(Groups)
STACKED <-stack(Groups)
STACKED[1:5,]
boxplot(values~ind,col=c("red","blue","green"),data=STACKED)
anova(lm(values~ind,data=STACKED))
remove(STACKED)
detach(Groups)
}
\keyword{datasets}
