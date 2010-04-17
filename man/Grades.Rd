\name{Grades}
\alias{Grades}
\docType{data}
\title{Test grades in a beginning statistics class}
\description{
  Data for Exercise 1.120
}
\usage{Grades}
\format{
  A data frame with 29 observations on the following variable.
  \describe{
    \item{\code{grades}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Grades)
attach(Grades)
EDA(grades)
detach(Grades)
}
\keyword{datasets}
