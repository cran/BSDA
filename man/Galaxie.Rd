\name{Galaxie}
\alias{Galaxie}
\docType{data}
\title{Velocities of 82 galaxies in the Corona Borealis region}
\description{
  Data for Example 5.2
}
\usage{Galaxie}
\format{
  A data frame with 82 observations on the following variable.
  \describe{
    \item{\code{velocity}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Galaxie)
attach(Galaxie)
EDA(velocity)
detach(Galaxie)
}
\keyword{datasets}
