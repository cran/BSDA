\name{Region}
\alias{Region}
\docType{data}
\title{Pollution index taken in three regions of the country}
\description{
  Data for Exercise 10.26
}
\usage{Region}
\format{
  A data frame with 48 observations on the following 6 variables.
  \describe{
    \item{\code{West}}{a numeric vector}
    \item{\code{Central}}{a numeric vector}
    \item{\code{East}}{a numeric vector}
    \item{\code{Index}}{a numeric vector}
    \item{\code{Region}}{a numeric vector}
    \item{\code{Ranks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Region)
attach(Region)
boxplot(Index~Region)
anova(lm(Index~as.factor(Region)))
detach(Region)
}
\keyword{datasets}
