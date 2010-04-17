\name{Anxiety}
\alias{Anxiety}
\docType{data}

\title{Math test scores versus anxiety scores before the test}

\description{Data used in Exercise 2.96}

\usage{Anxiety}

\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{anxiety}}{a numeric vector}
    \item{\code{math}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
attach(Anxiety)
plot(anxiety,math)
cor(anxiety,math)
linmod <- lm(math~anxiety)
abline(linmod)
summary(linmod)
detach(Anxiety)
}

\keyword{datasets}
