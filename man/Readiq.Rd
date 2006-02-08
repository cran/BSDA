\name{Readiq}
\alias{Readiq}
\docType{data}
\title{ Reading scores versus IQ scores}
\description{
  Data for Exercises 2.10 and 2.53
}
\usage{Readiq}
\format{
  A data frame with 14 observations on the following 2 variables.
  \describe{
    \item{\code{reading}}{a numeric vector}
    \item{\code{IQ}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Readiq)
attach(Readiq)
plot(IQ,reading)
model <- lm(reading~IQ)
abline(model)
detach(Readiq)
}
\keyword{datasets}
