\name{Diplomat}
\alias{Diplomat}
\docType{data}
\title{Parking tickets issued to diplomats}
\description{
  Data for Exercises 1.14 and 1.47
}
\usage{Diplomat}
\format{
  A data frame with 10 observations on the following 4 variables.
  \describe{
    \item{\code{Country}}{a factor with levels \code{Brazil} \code{Bulgaria} \code{Egypt} \code{Indonesia} \code{Israel} \code{Nigeria} \code{Russia} \code{S. Korea} \code{Ukraine} \code{Venezuela}}
    \item{\code{Number}}{a numeric vector}
    \item{\code{rate}}{a numeric vector}
    \item{\code{Code}}{a factor with levels \code{Br} \code{Bu} \code{Eg} \code{In} \code{Is} \code{Ni} \code{Ru} \code{SK} \code{Uk} \code{Ve}}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Diplomat)
attach(Diplomat)
par(mfrow=c(1,2))
names(Number) <- Country
dotchart(Number,main="Number of Tickets",col="blue",pch=1)
names(rate) <- Country
dotchart(rate,main="Tickets/Vehicle/Month",col="red",pch=2)
barplot(rate,col="pink")
detach(Diplomat)
}
\keyword{datasets}
