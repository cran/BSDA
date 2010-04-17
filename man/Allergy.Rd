\name{Allergy}
\alias{Allergy}
\docType{data}

\title{Allergy medicines by adverse events}

\description{Data used in Exercise 8.22}

\usage{Allergy}

\format{
  A data frame with 3 observations on the following 4 variables.
  \describe{
    \item{\code{C1.T}}{a factor with levels \code{Drowsiness} \code{Headache} \code{Insomnia}}
    \item{\code{Seldane}}{a numeric vector}
    \item{\code{Pseudoep}}{a numeric vector}
    \item{\code{Placebo}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
attach(Allergy)
X <- as.matrix(Allergy[1:3,2:4])
chisq.test(X)
Xr <- as.matrix(Allergy[2:3,2:4])
chisq.test(Xr)
detach(Allergy)
}

\keyword{datasets}
