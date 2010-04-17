\name{Censored}
\alias{Censored}
\docType{data}
\title{Entry age and survival time of patients with small cell lung cancer under two different treatments}
\description{
  Data for Exercises 7.34 and 7.48 
}
\usage{Censored}
\format{
  A data frame with 121 observations on the following 8 variables.
  \describe{
    \item{\code{survivA}}{a numeric vector}
    \item{\code{ageA}}{a numeric vector}
    \item{\code{censorA}}{a numeric vector}
    \item{\code{survivB}}{a numeric vector}
    \item{\code{ageB}}{a numeric vector}
    \item{\code{censorB}}{a numeric vector}
    \item{\code{survival}}{a numeric vector}
    \item{\code{group}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Censored)
attach(Censored)
boxplot(survival~group)
detach(Censored)
}
\keyword{datasets}
