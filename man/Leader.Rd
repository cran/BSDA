\name{Leader}
\alias{Leader}
\docType{data}
\title{Leadership exam scores by age for employees on an industrial plant}
\description{
  Data for Exercise 7.31
}
\usage{Leader}
\format{
  A data frame with 34 observations on the following 2 variables.
  \describe{
    \item{\code{under35}}{a numeric vector}
    \item{\code{over35}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Leader)
attach(Leader)
boxplot(under35,over35,names=c("Under 35","Over 35"),col=c("green","brown"))
t.test(under35,over35)
detach(Leader)
}
\keyword{datasets}
