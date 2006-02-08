\name{Dyslexia}
\alias{Dyslexia}
\docType{data}
\title{Data on a group of college students diagnosed with dyslexia}
\description{
  Data for Exercise 2.90
}
\usage{Dyslexia}
\format{
  A data frame with 8 observations on the following 7 variables.
  \describe{
    \item{\code{words}}{a numeric vector}
    \item{\code{age}}{a numeric vector}
    \item{\code{gender}}{a factor with levels \code{f} \code{m}}
    \item{\code{handed}}{a factor with levels \code{l} \code{r}}
    \item{\code{weight}}{a numeric vector}
    \item{\code{height}}{a numeric vector}
    \item{\code{children}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Dyslexia)
attach(Dyslexia)
plot(weight,height)
plot(handed,words)
detach(Dyslexia)
}
\keyword{datasets}
