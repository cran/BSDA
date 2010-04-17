\name{Firstchi}
\alias{Firstchi}
\docType{data}
\title{Ages of women at the birth of their first child}
\description{
  Data for Exercise 5.11
}
\usage{Firstchi}
\format{
  A data frame with 87 observations on the following variable.
  \describe{
    \item{\code{age}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Firstchi)
attach(Firstchi)
EDA(age)
detach(Firstchi)
}
\keyword{datasets}
