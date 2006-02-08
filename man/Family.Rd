\name{Family}
\alias{Family}
\docType{data}
\title{Size of family versus cost per person per week for groceries}
\description{
  Data for Exercise 2.89
}
\usage{Family}
\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{Number}}{a numeric vector}
    \item{\code{Cost}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Family)
attach(Family)
plot(Number,Cost)
cor(Number,Cost)
lm(Cost~Number)
detach(Family)
}
\keyword{datasets}
