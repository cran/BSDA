\name{Commute}
\alias{Commute}
\docType{data}
\title{Commuting times for selected cities in 1980 and 1990}
\description{
  Data for Exercises 1.13, and 7.85
}
\usage{Commute}
\format{
  A data frame with 39 observations on the following 3 variables.
  \describe{
    \item{\code{City}}{a factor with levels \code{Atlanta} \code{Baltimore} \code{Boston} \code{Buffalo} \code{Charlotte} \code{Chicago} \code{Cincinnati} \code{Cleveland} \code{Columbus} \code{Dallas} \code{Denver} \code{Detroit} \code{Hartford} \code{Houston} \code{Indianapolis} \code{Kansas City} \code{Los Angeles} \code{Miami} \code{Milwaukee} \code{Minneapolis} \code{New Orleans} \code{New York} \code{Norfolk} \code{Orlando} \code{Philadelphia} \code{Phoenix} \code{Pittsburgh} \code{Portland} \code{Providence} \code{Rochester} \code{Sacramento} \code{Salt Lake City} \code{San Antonio} \code{San Diego} \code{San Francisco} \code{Seattle} \code{St. Louis} \code{Tampa} \code{Washington}}
    \item{\code{X1980}}{a numeric vector}
    \item{\code{X1990}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Commute)
attach(Commute)
stripchart(x=list(X1980,X1990),method="stack",pch=1,cex=2,col=c("red","blue"),
group.names=c("1980","1990"),main="",xlab="minutes")
title(main="Commute Time")
boxplot(X1980,X1990,col=c("red","blue"),names=c("1980","1990"),horizontal=TRUE,las=1)
library(lattice)
commute <- stack(Commute)
commute[1:5,]
attach(commute)
stripplot(ind~values,jitter=TRUE)
dotplot(ind~values)
bwplot(ind~values)
remove(commute)
detach(Commute)
}
\keyword{datasets}
