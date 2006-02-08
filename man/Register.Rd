\name{Register}
\alias{Register}
\docType{data}
\title{Maintenance cost versus age of cash registers in a department store}
\description{
  Data for Exercise 2.3, 2.39, and 2.54
}
\usage{Register}
\format{
  A data frame with 9 observations on the following 4 variables.
  \describe{
    \item{\code{age}}{a numeric vector}
    \item{\code{cost}}{a numeric vector}
    \item{\code{SRES1}}{a numeric vector}
    \item{\code{FITS1}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Register)
attach(Register)
plot(age,cost,main="Exercise 2.3")
model <- lm(cost~age)
abline(model)
plot(age,resid(model))
detach(Register)
}
\keyword{datasets}
