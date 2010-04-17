\name{Submarin}
\alias{Submarin}
\docType{data}
\title{ Number of German submarines sunk by U.S. Navy in World War II}
\description{
  Data for Exercises 2.16, 2.45, and 2.59
}
\usage{Submarin}
\format{
  A data frame with 16 observations on the following 3 variables.
  \describe{
    \item{\code{Month}}{a numeric vector}
    \item{\code{reported}}{a numeric vector}
    \item{\code{actual}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Submarin)
attach(Submarin)
plot(reported,actual)
model <- lm(actual~reported)
abline(model)
anova(model)
summary(model)
detach(Submarin)
}
\keyword{datasets}
