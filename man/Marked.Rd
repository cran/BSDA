\name{Marked}
\alias{Marked}
\docType{data}
\title{Percent of marked cars in 65 police departments in Florida}
\description{
  Data for Exercise 6.100
}
\usage{Marked}
\format{
  A data frame with 65 observations on the following variable.
  \describe{
    \item{\code{percent}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Marked)
attach(Marked)
EDA(percent)
t.test(percent,mu=60,alternative="greater")
SIGN.test(percent,md=60,alternative="greater")
detach(Marked)
}
\keyword{datasets}
