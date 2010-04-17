\name{Track15}
\alias{Track15}
\docType{data}
\title{Olympic winning times for the men's 1500-meter run}
\description{
  Data for Exercise 1.36
}
\usage{Track15}
\format{
  A data frame with 26 observations on the following 2 variables.
  \describe{
    \item{\code{Year}}{a numeric vector}
    \item{\code{X1500m}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Track15)
attach(Track15)
plot(Year,X1500m,type="l",lwd=2,lty=2,col="red",xlab="Year",
ylab="1500m Time (seconds)",main="Problem 1.36")       
detach(Track15) 
}
\keyword{datasets}
