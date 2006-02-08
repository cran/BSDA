\name{Miller1}
\alias{Miller1}
\docType{data}
\title{Twenty scores on the Miller personality test}
\description{
  Data for Exercise 1.41
}
\usage{Miller1}
\format{
  A data frame with 20 observations on the following variable.
  \describe{
    \item{\code{miller}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Miller1)
attach(Miller1)
stem(miller)
stem(miller,scale=2)
detach(Miller1)
}
\keyword{datasets}
