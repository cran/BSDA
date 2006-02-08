\name{Shuttle}
\alias{Shuttle}
\docType{data}
\title{ Number of people riding shuttle versus number of automobiles in the downtown area}
\description{
  Data for Exercise 9.20
}
\usage{Shuttle}
\format{
  A data frame with 15 observations on the following 2 variables.
  \describe{
    \item{\code{shuttle}}{a numeric vector}
    \item{\code{autos}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Shuttle)
attach(Shuttle)
model <- lm(autos~shuttle)
summary(model)
detach(Shuttle)
remove(model)
}
\keyword{datasets}
