\name{Test1}
\alias{Test1}
\docType{data}
\title{ Grades on the first test in a statistics class}
\description{
  Data for Exercise 5.80
}
\usage{Test1}
\format{
  A data frame with 25 observations on the following variable.
  \describe{
    \item{\code{test1}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Test1)
attach(Test1)
EDA(test1)
detach(Test1)
}
\keyword{datasets}
