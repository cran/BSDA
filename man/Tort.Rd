\name{Tort}
\alias{Tort}
\docType{data}
\title{The number of torts, average number of months to process a tort, and county population from the court files of the nation's largest counties}
\description{
  Data for Exercise 5.13
}
\usage{Tort}
\format{
  A data frame with 45 observations on the following 5 variables.
  \describe{
    \item{\code{county}}{a factor with levels \code{alameda, ca} \code{allegheny, pa} \code{bergen, nj} \code{bexar, tx} \code{contra costa, ca} \code{cook, il} \code{cuyahoga, oh} \code{dade, fl} \code{dallas, tx} \code{dupage, il} \code{essex, ma} \code{essex, nj} \code{fairfax, va} \code{fairfield, ct} \code{franklin, oh} \code{fresno, ca} \code{fulton, ga} \code{harris, tx} \code{hartford, ct} \code{hennepin, mn} \code{honolulu, hi} \code{jefferson, ky} \code{king, wa} \code{los angeles, ca} \code{maricopa, az} \code{marion, in} \code{middlesex, ma} \code{middlesex, nj} \code{milwaukee, wi} \code{new york, ny} \code{norfolk, ma} \code{oakland, mi} \code{orange, ca} \code{orange, fl} \code{palm beach, fl} \code{philadelphia, pa} \code{pima, az} \code{san bernadino, ca} \code{san francisco, ca} \code{santa clara, ca} \code{st. louis, mo} \code{suffolk, ma} \code{ventura, ca} \code{wayne, mi} \code{worchester, ma}}
    \item{\code{months}}{a numeric vector}
    \item{\code{populat}}{a numeric vector}
    \item{\code{torts}}{a numeric vector}
    \item{\code{rate}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Tort)
attach(Tort)
EDA(months)
detach(Tort)
}
\keyword{datasets}
