\name{Life}
\alias{Life}
\docType{data}
\title{Life expectancy of men and women in U.S.}
\description{
  Data for Exercise 1.31
}
\usage{Life}
\format{
  A data frame with 8 observations on the following 3 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{Men}}{a numeric vector}
    \item{\code{Women}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Life)
attach(Life)
plot(year,Men,type="l",ylim=c(min(Men,Women),max(Men,Women)),col="blue",
main="Life Expectancy versus Year",ylab="Age",xlab="Year")
lines(year,Women,col="red")
text(1955,65,"Men",col="blue")
text(1955,70,"Women",col="red")
detach(Life)
}
\keyword{datasets}
