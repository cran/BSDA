\name{Victoria}
\alias{Victoria}
\docType{data}
\title{Number of sunspots versus mean annual level of Lake Victoria Nyanza from 1902 to 1921}
\description{
  Data for Exercise 2.98
}
\usage{Victoria}
\format{
  A data frame with 20 observations on the following 3 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{level}}{a numeric vector}
    \item{\code{sunspot}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Victoria)
attach(Victoria)
plot(sunspot,level)
model <- lm(level~sunspot)
abline(model)
cor(sunspot,level)
model
detach(Victoria)
}
\keyword{datasets}
