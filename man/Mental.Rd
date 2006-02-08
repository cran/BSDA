\name{Mental}
\alias{Mental}
\docType{data}
\title{Median mental ages of 16 girls}
\description{
  Data for Exercise 6.52
}
\usage{Mental}
\format{
  A data frame with 16 observations on the following variable.
  \describe{
    \item{\code{age}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Mental)
attach(Mental)
sign.test(age,md=100)
detach(Mental)
}
\keyword{datasets}
