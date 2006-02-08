\name{Soccer}
\alias{Soccer}
\docType{data}
\title{ Weights  of 25 soccer players}
\description{
  Data for Exercise 1.46
}
\usage{Soccer}
\format{
  A data frame with 25 observations on the following variable.
  \describe{
    \item{\code{weight}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Soccer)
attach(Soccer)
stem(weight,scale=2)
hist(weight,breaks=seq(110,210,10),col="orange",
main="Problem 1.46 \n Weights of Soccer Players",right=FALSE)
detach(Soccer)
}
\keyword{datasets}
