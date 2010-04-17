\name{Short}
\alias{Short}
\docType{data}
\title{James Short's measurements of the parallax of the sun}
\description{
  Data for Exercise 6.65
}
\usage{Short}
\format{
  A data frame with 158 observations on the following 10 variables.
  \describe{
    \item{\code{Sample.1}}{a numeric vector}
    \item{\code{Sample.2}}{a numeric vector}
    \item{\code{Sample.3}}{a numeric vector}
    \item{\code{Sample.4}}{a numeric vector}
    \item{\code{Sample.5}}{a numeric vector}
    \item{\code{Sample.6}}{a numeric vector}
    \item{\code{Sample.7}}{a numeric vector}
    \item{\code{Sample.8}}{a numeric vector}
    \item{\code{Parallax}}{a numeric vector}
    \item{\code{Sample}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Short)
attach(Short)
hist(Parallax)
EDA(Parallax)
SIGN.test(Parallax,md=8.798)
t.test(Parallax,mu=8.798)
detach(Short)
}
\keyword{datasets}
