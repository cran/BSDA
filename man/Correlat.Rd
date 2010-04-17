\name{Correlat}
\alias{Correlat}
\docType{data}
\title{Exercise to illustrate correlation}
\description{
  Data for Exercise 2.23
}
\usage{Correlat}
\format{
  A data frame with 13 observations on the following 2 variables.
  \describe{
    \item{\code{X}}{a numeric vector}
    \item{\code{Y}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Correlat)
attach(Correlat)
plot(X,Y)
model <- lm(Y~X)
abline(model)
detach(Correlat)
}
\keyword{datasets}
