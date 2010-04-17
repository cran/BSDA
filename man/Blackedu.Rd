\name{Blackedu}
\alias{Blackedu}
\docType{data}
\title{ Education level of blacks by gender}
\description{
  Data for Exercise 8.55
}
\usage{Blackedu}
\format{
  A data frame with 5 observations on the following 3 variables.
  \describe{
    \item{\code{education}}{a factor with levels \code{bachelor deg} \code{graduate deg} \code{high sch dropout} \code{high sch graduate} \code{some college}}
    \item{\code{female}}{a numeric vector}
    \item{\code{male}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Blackedu)
attach(Blackedu)
Blackedu
chisq.test(Blackedu[,2:3])
detach(Blackedu)
}
\keyword{datasets}
