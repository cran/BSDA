\name{Newsstand}
\alias{Newsstand}
\docType{data}
\title{Daily profits for 20 newsstands}
\description{
  Data for Exercise 1.43
}
\usage{Newsstand}
\format{
  A data frame with 20 observations on the following variable.
  \describe{
    \item{\code{profit}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Newsstand)
attach(Newsstand)
stem(profit)
stem(profit,scale=3)
detach(Newsstand)
}
\keyword{datasets}
