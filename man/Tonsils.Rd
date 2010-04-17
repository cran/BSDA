\name{Tonsils}
\alias{Tonsils}
\docType{data}
\title{Size of tonsils collected from 1,398 children}
\description{
  Data for Exercise 2.78
}
\usage{Tonsils}
\format{
  A data frame with 3 observations on the following 3 variables.
  \describe{
    \item{\code{Size}}{a factor with levels \code{Large} \code{Normal} \code{Very Large}}
    \item{\code{Carrier}}{a numeric vector}
    \item{\code{Non.carrier}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Tonsils)
attach(Tonsils)
TON <- as.matrix(Tonsils[,2:3])
rownames(TON) <- Size
TON
barplot(t(TON),beside=TRUE,legend=TRUE)
remove(TON)
detach(Tonsils)
}
\keyword{datasets}
