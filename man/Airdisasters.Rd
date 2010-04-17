\name{Airdisasters}
\alias{Airdisasters}
\docType{data}

\title{Aircraft disasters in five different decades}

\description{Data used in Exercise 1.12}

\usage{Airdisasters}

\format{
  A data frame with 141 observations on the following 7 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{deaths}}{a numeric vector}
    \item{\code{X1950}}{a numeric vector}
    \item{\code{X1960}}{a numeric vector}
    \item{\code{X1970}}{a numeric vector}
    \item{\code{X1980}}{a numeric vector}
    \item{\code{X1990}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
attach(Airdisasters)
STA <- stack(Airdisasters[,3:7])
library(lattice)
dotplot(ind~values,data=STA)
stripchart(x=list(X1950,X1960,X1970,X1980,X1990),method="stack",main="",pch=1,
col="red",group.names=c("1950","1960","1970","1980","1990"),
xlab="Number of Fatalities")
title(main="Aircraft Disasters 1950-1990") 
detach(Airdisasters)
}

\keyword{datasets}
