\name{Lethal}
\alias{Lethal}
\docType{data}
\title{Survival time of mice injected with an experimental lethal drug}
\description{
  Data for Example 6.12
}
\usage{Lethal}
\format{
  A data frame with 30 observations on the following variable.
  \describe{
    \item{\code{survival}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Lethal)
attach(Lethal)
sign.test(survival,md=45,alternative="less")
detach(Lethal)
}
\keyword{datasets}
