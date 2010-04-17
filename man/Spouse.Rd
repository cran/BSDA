\name{Spouse}
\alias{Spouse}
\docType{data}
\title{Convictions in spouse murder cases by gender}
\description{
  Data for Exercise 8.33
}
\usage{Spouse}
\format{
  A data frame with 4 observations on the following 3 variables.
  \describe{
    \item{\code{result}}{a factor with levels \code{acquitted} \code{convicted} \code{not prosecuted} \code{pleaded guilty}}
    \item{\code{husband}}{a numeric vector}
    \item{\code{wife}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Spouse)
attach(Spouse)
Spouse
chisq.test(Spouse[,2:3])
detach(Spouse)
}
\keyword{datasets}
