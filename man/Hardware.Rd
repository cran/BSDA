\name{Hardware}
\alias{Hardware}
\docType{data}
\title{Daily receipts for a small hardware store for 31 working days}
\description{
  Data for unknown problem
}
\usage{Hardware}
\format{
  A data frame with 31 observations on the following variable.
  \describe{
    \item{\code{receipt}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Hardware)
attach(Hardware)
stem(receipt)
detach(Hardware)
}
\keyword{datasets}
