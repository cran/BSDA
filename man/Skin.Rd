\name{Skin}
\alias{Skin}
\docType{data}
\title{ Survival times of closely and poorly matched skin grafts on burn patients}
\description{
  Data for Exercise 5.20
}
\usage{Skin}
\format{
  A data frame with 11 observations on the following 2 variables.
  \describe{
    \item{\code{close}}{a numeric vector}
    \item{\code{poor}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Skin)
attach(Skin)
DIFF <- close-poor
stem(DIFF)
EDA(DIFF)
remove(DIFF)
detach(Skin)
}
\keyword{datasets}
