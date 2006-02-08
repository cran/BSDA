\name{Weather94}
\alias{Weather94}
\docType{data}
\title{Weather casualties in 1994}
\description{
  Data for Exercise 1.30
}
\usage{Weather94}
\format{
  A data frame with 11 observations on the following 2 variables.
  \describe{
    \item{\code{Weather.Type}}{a factor with levels \code{Extreme Temp} \code{Flash flood} \code{Fog} \code{High wind} \code{Hurricane} \code{Lightning} \code{Other} \code{River flood} \code{Thunderstorm} \code{Tornado} \code{Winter weather}}
    \item{\code{Number}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Weather94)
attach(Weather94)
names(Number) <- Weather.Type
barplot(Number,col="lightblue",las=2,cex.names=.65,main="Problem 1.30") 
# las=2 places bar names vertically
detach(Weather94)
}
\keyword{datasets}
