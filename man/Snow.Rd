\name{Snow}
\alias{Snow}
\docType{data}
\title{Concentration of microparticles in snowfields of Greenland and Antarctica}
\description{
  Data for Exercise 7.87
}
\usage{Snow}
\format{
  A data frame with 34 observations on the following 4 variables.
  \describe{
    \item{\code{antarc}}{a numeric vector}
    \item{\code{greenld}}{a numeric vector}
    \item{\code{concent}}{a numeric vector}
    \item{\code{site}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Snow)
attach(Snow)
boxplot(concent~site)
detach(Snow)
}
\keyword{datasets}
