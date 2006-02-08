\name{Malpract}
\alias{Malpract}
\docType{data}
\title{Amounts awarded in 17 malpractice cases}
\description{
  Data for Exercise 5.73
}
\usage{Malpract}
\format{
  A data frame with 17 observations on the following variable.
  \describe{
    \item{\code{award}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Malpract)
attach(Malpract)
sign.test(award,conf.level=.90)
detach(Malpract)
}
\keyword{datasets}
