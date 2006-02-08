\name{Mathcomp}
\alias{Mathcomp}
\docType{data}
\title{Standardized math competency for a group of entering freshmen at a small community college}
\description{
  Data for Exercise 5.26
}
\usage{Mathcomp}
\format{
  A data frame with 31 observations on the following variable.
  \describe{
    \item{\code{score}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Mathcomp)
attach(Mathcomp)
stem(score)
EDA(score)
detach(Mathcomp)
}
\keyword{datasets}
