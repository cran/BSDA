\name{Senior}
\alias{Senior}
\docType{data}
\title{Reaction times of 30 senior citizens applying for drivers license renewals}
\description{
  Data for Exercise 1.83 and 3.67
}
\usage{Senior}
\format{
  A data frame with 31 observations on the following variable.
  \describe{
    \item{\code{reaction}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Senior)
attach(Senior)
fivenum(reaction)
boxplot(reaction,horizontal=TRUE,main="Problem 1.83 Part d.",col="orange")
detach(Senior)
}
\keyword{datasets}
