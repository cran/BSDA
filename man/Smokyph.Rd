\name{Smokyph}
\alias{Smokyph}
\docType{data}
\title{Water pH levels of 75 water samples taken in the Great Smoky Mountains}
\description{
  Data for Exercises 6.40, 6.59, 7.10, and 7.35
}
\usage{Smokyph}
\format{
  A data frame with 75 observations on the following 5 variables.
  \describe{
    \item{\code{waterph}}{a numeric vector}
    \item{\code{code}}{a factor with levels \code{high} \code{low}}
    \item{\code{elev}}{a numeric vector}
    \item{\code{SRES1}}{a numeric vector}
    \item{\code{FITS1}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Smokyph)
attach(Smokyph)
t.test(waterph,mu=7)
SIGN.test(waterph,md=7)
tapply(waterph,code,mean)
stripchart(waterph~code,method="stack",pch=19,col=c("red","blue"))
qqnorm(waterph[code=="low"])
qqnorm(waterph[code=="high"])
t.test(waterph[code=="low"],waterph[code=="high"])
t.test(waterph[code=="low"],waterph[code=="high"],conf.level=.90)$conf
detach(Smokyph)
}
\keyword{datasets}
