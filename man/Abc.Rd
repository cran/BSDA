\name{Abc}
\alias{Abc}
\docType{data}

\title{Three samples to illustrate analysis of variance}

\description{Data used in Exercise 10.1}

\usage{Abc}

\format{
  A data frame with 18 observations on the following 3 variables.
  \describe{
    \item{GroupA}{a numeric vector}
    \item{GroupB}{a numeric vector}
    \item{GroupC}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
attach(Abc)
STACKED <-stack(Abc)
STACKED[1:5,]
boxplot(values~ind,col=c("red","blue","green"),data=STACKED)
anova(lm(values~ind,data=STACKED))
remove(STACKED)
detach(Abc)
}
\keyword{datasets}
