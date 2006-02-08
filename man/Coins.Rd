\name{Coins}
\alias{Coins}
\docType{data}
\title{Yearly returns on 12 investments}
\description{
  Data for Exercise 5.68
}
\usage{Coins}
\format{
  A data frame with 12 observations on the following variable.
  \describe{
    \item{\code{coins}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Coins)
attach(Coins)
qqnorm(coins)
qqline(coins)
EDA(coins)
detach(Coins)
}
\keyword{datasets}
