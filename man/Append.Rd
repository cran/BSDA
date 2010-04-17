\name{Append}
\alias{Append}
\docType{data}

\title{Median costs of an appendectomy at 20 hospitals in North Carolina}

\description{Data for Exercise 1.119}

\usage{Append}

\format{
  A data frame with 20 observations on the following variable.
  \describe{
    \item{\code{fee}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Append)
attach(Append)
ll <- mean(fee)-2*sd(fee)
ul <- mean(fee)+2*sd(fee)
limits <-c(ll,ul)
limits
fee[fee<ll | fee>ul]
detach(Append)
}

\keyword{datasets}
