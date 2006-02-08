\name{Fluid}
\alias{Fluid}
\docType{data}
\title{Breakdown times of an insulating fluid under various levels of voltage stress}
\description{
  Data for Exercise 5.76
}
\usage{Fluid}
\format{
  A data frame with 76 observations on the following 10 variables.
  \describe{
    \item{\code{X26kV}}{a numeric vector}
    \item{\code{X28kV}}{a numeric vector}
    \item{\code{X30kV}}{a numeric vector}
    \item{\code{X32kV}}{a numeric vector}
    \item{\code{X34kV}}{a numeric vector}
    \item{\code{X36kV}}{a numeric vector}
    \item{\code{X38kV}}{a numeric vector}
    \item{\code{response}}{a numeric vector}
    \item{\code{group}}{a numeric vector}
    \item{\code{ln.resp.}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Fluid)
attach(Fluid)
stem(X34kV)
sign.test(X34kV)
detach(Fluid)
}
\keyword{datasets}
