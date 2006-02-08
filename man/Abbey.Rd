\name{Abbey}
\alias{Abbey}
\docType{data}

\title{Daily price returns (in pence) of Abbey National shares between 7/31/91 and 10/8/91}

\description{Data used in problem 6.39}

\usage{Abbey}
\format{
  A data frame with 50 observations on the following variable.
  \describe{
    \item{\code{C1}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
attach(Abbey)
EDA(C1)
t.test(C1,mu=300)
detach(Abbey)
}
\keyword{datasets}
