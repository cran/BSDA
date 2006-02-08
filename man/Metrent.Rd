\name{Metrent}
\alias{Metrent}
\docType{data}
\title{Monthly rental costs in metro areas with 1 million or more persons}
\description{
  Data for Exercise 5.117
}
\usage{Metrent}
\format{
  A data frame with 46 observations on the following variable.
  \describe{
    \item{\code{rent}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Metrent)
attach(Metrent)
EDA(rent)
t.test(rent,conf.level=.99)$conf
detach(Metrent)
}
\keyword{datasets}
