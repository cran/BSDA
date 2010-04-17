\name{Moisture}
\alias{Moisture}
\docType{data}
\title{Moisture content and depth of core sample for marine muds in eastern Louisiana }
\description{
  Data for Exercise 9.37
}
\usage{Moisture}
\format{
  A data frame with 16 observations on the following 4 variables.
  \describe{
    \item{\code{depth}}{a numeric vector}
    \item{\code{moisture}}{a numeric vector}
    \item{\code{lnmoist}}{a numeric vector}
    \item{\code{depthsq}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Moisture)
attach(Moisture)
model <- lm(moisture~depth)
plot(depth,resid(model))
detach(Moisture)
remove(model)
}
\keyword{datasets}
