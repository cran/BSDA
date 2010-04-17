\name{Defectiv}
\alias{Defectiv}
\docType{data}
\title{Number of defective items produced by 20 employees}
\description{
  Data for Exercise 1.27
}
\usage{Defectiv}
\format{
  A data frame with 20 observations on the following 4 variables.
  \describe{
    \item{\code{C1}}{a numeric vector}
    \item{\code{number}}{a numeric vector}
    \item{\code{Count}}{a numeric vector}
    \item{\code{Percent}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Defectiv)
attach(Defectiv)
table(C1)
barplot(table(C1),col="pink",ylab="Frequency",
xlab="Defective Items Produced by Employees",main="Problem 1.27")
detach(Defectiv)
}
\keyword{datasets}
