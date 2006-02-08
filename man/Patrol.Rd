\name{Patrol}
\alias{Patrol}
\docType{data}
\title{Years of experience and number of tickets given by patrolpersons in New York City}
\description{
  Data for Example 9.3
}
\usage{Patrol}
\format{
  A data frame with 10 observations on the following 7 variables.
  \describe{
    \item{\code{tickets}}{a numeric vector}
    \item{\code{years}}{a numeric vector}
    \item{\code{ln.tickets.}}{a numeric vector}
    \item{\code{SRES1}}{a numeric vector}
    \item{\code{FITS1}}{a numeric vector}
    \item{\code{SRES2}}{a numeric vector}
    \item{\code{FITS2}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Patrol)
attach(Patrol)
model <- lm(tickets~years)
summary(model)
detach(Patrol)
remove(model)
}
\keyword{datasets}
