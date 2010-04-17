\name{Detroit}
\alias{Detroit}
\docType{data}
\title{ Educational levels of a sample of 40 auto workers in Detroit}
\description{
  Data for Exercise 5.21
}
\usage{Detroit}
\format{
  A data frame with 40 observations on the following variable.
  \describe{
    \item{\code{educ}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Detroit)
attach(Detroit)
EDA(educ)
detach(Detroit)
}
\keyword{datasets}
