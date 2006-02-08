\name{Freshman}
\alias{Freshman}
\docType{data}
\title{Ages of a random sample of 30 college freshmen}
\description{
  Data for Exercise 6.53
}
\usage{Freshman}
\format{
  A data frame with 30 observations on the following variable.
  \describe{
    \item{\code{age}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Freshman)
attach(Freshman)
sign.test(age,md=19)
detach(Freshman)
}
\keyword{datasets}
