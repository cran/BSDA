\name{Aggress}
\alias{Aggress}
\docType{data}

\title{Agressive tendency scores for a group of teenage members of a street gang}

\description{Data used in Exercises 1.61 and 1.81}

\usage{Aggress}

\format{
  A data frame with 28 observations on the following variable.
  \describe{
    \item{\code{aggres}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Aggress)
attach(Aggress)
EDA(aggres)
# OR
IQR(aggres)
diff(range(aggres))
detach(Aggress)
}

\keyword{datasets}
