\name{Dowjones}
\alias{Dowjones}
\docType{data}
\title{Closing yearend Dow Jones Industrial averages from 1896 through 2000}
\description{
  Data for Exercise 1.35
}
\usage{Dowjones}
\format{
  A data frame with 105 observations on the following 3 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{close}}{a numeric vector}
    \item{\code{X.change}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Dowjones)
attach(Dowjones)  
plot(year,close,type="l",lty=2,lwd=2,col="blue")     
barplot(close,col="blue",las=2,main="Problem 1.35",names.arg=FALSE)      
detach(Dowjones) 
}
\keyword{datasets}
