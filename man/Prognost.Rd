\name{Prognost}
\alias{Prognost}
\docType{data}
\title{Klopfer's prognostic rating scale for subjects receiving behavior modification therapy}
\description{
  Data for Exercise 6.61
}
\usage{Prognost}
\format{
  A data frame with 15 observations on the following variable.
  \describe{
    \item{\code{score}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Prognost)
attach(Prognost)
EDA(score)
t.test(score,mu=9)
detach(Prognost)
}
\keyword{datasets}
