\name{Schizop2}
\alias{Schizop2}
\docType{data}
\title{ Exam scores for 17 patients to assess the learning ability of schizophrenics after taking a specified does of a tranquilizer}
\description{
  Data for Exercise 6.99
}
\usage{Schizop2}
\format{
  A data frame with 17 observations on the following variable.
  \describe{
    \item{\code{score}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Schizop2)
attach(Schizop2)
EDA(score)
sign.test(score,md=22,alternative="greater")
detach(Schizop2)
}
\keyword{datasets}
