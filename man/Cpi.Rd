\name{Cpi}
\alias{Cpi}
\docType{data}
\title{Consumer price index from 1979 to 1998}
\description{
  Data for Exercise 1.34
}
\usage{Cpi}
\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{Year}}{a numeric vector}
    \item{\code{CPI}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Cpi)
attach(Cpi)  
plot(Year,CPI,type="l",lty=2,lwd=2,col="red")   
names(CPI) <- Year
barplot(CPI,col="pink",las=2,main="Problem 1.34")   
detach(Cpi)
}
\keyword{datasets}
