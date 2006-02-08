\name{Reaction}
\alias{Reaction}
\docType{data}
\title{Threshold reaction time for persons subjected to emotional stress}
\description{
  Data for Example 6.11
}
\usage{Reaction}
\format{
  A data frame with 12 observations on the following variable.
  \describe{
    \item{\code{time}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Reaction)
attach(Reaction)
sign.test(time,md=15,alternative="less")
detach(Reaction)
}
\keyword{datasets}
