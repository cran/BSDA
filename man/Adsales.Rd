\name{Adsales}
\alias{Adsales}
\docType{data}

\title{Number of ads versus number of sales for a retailer of satellite dishes}

\description{Data used in Exercise 9.15}

\usage{Adsales}

\format{
  A data frame with 6 observations on the following 2 variables.
  \describe{
    \item{\code{ads}}{a numeric vector}
    \item{\code{sales}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}


\examples{
attach(Adsales)
plot(ads,sales)
linmod <- lm(sales~ads)
abline(linmod)
summary(linmod)
detach(Adsales)
}

\keyword{datasets}
