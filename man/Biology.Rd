\name{Biology}
\alias{Biology}
\docType{data}
\title{Test scores on first exam in biology class}
\description{
  Data for Exercise 1.49
}
\usage{Biology}
\format{
  A data frame with 30 observations on the following variable.
  \describe{
    \item{\code{score}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Biology)
attach(Biology)
hist(score,breaks="scott",col="brown",prob=TRUE,main="Problem 1.49")
lines(density(score),lwd=3)
detach(Biology)
}
\keyword{datasets}
