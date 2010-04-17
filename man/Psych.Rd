\name{Psych}
\alias{Psych}
\docType{data}
\title{Correct responses for 24 students in a psychology experiment}
\description{
  Data for Exercise 1.42
}
\usage{Psych}
\format{
  A data frame with 23 observations on the following variable.
  \describe{
    \item{\code{score}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Psych)
attach(Psych)
stem(score)
EDA(score)
detach(Psych)
}
\keyword{datasets}
