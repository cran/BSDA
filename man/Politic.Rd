\name{Politic}
\alias{Politic}
\docType{data}
\title{Political party and gender in a voting district}
\description{
  Data for Example 8.3
}
\usage{Politic}
\format{
  A data frame with 250 observations on the following 2 variables.
  \describe{
    \item{\code{Party}}{a numeric vector}
    \item{\code{Gender}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Politic)
attach(Politic)
table(Party,Gender)
chisq.test(table(Party,Gender))
detach(Politic)
}
\keyword{datasets}
