\name{Schizoph}
\alias{Schizoph}
\docType{data}
\title{ Standardized exam scores for 13 patients to investigate the learning ability of schizophrenics after a specified dose of a tranquilizer}
\description{
  Data for Example 6.10
}
\usage{Schizoph}
\format{
  A data frame with 13 observations on the following variable.
  \describe{
    \item{\code{score}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Schizoph)
attach(Schizoph)
EDA(score)
t.test(score,mu=20)
detach(Schizoph)
}
\keyword{datasets}
