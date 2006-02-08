\name{Social}
\alias{Social}
\docType{data}
\title{Median income level for 25 social workers from North Carolina}
\description{
  Data for Exercise 6.63
}
\usage{Social}
\format{
  A data frame with 25 observations on the following variable.
  \describe{
    \item{\code{income}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Social)
attach(Social)
sign.test(income,md=27500,alternative="less")
detach(Social)
}
\keyword{datasets}