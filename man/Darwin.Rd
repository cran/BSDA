\name{Darwin}
\alias{Darwin}
\docType{data}
\title{Charles Darwin's study of cross-fertilized and self-fertilized plants}
\description{
  Data for Exercise 7.62
}
\usage{Darwin}
\format{
  A data frame with 30 observations on the following 7 variables.
  \describe{
    \item{\code{pot}}{a numeric vector}
    \item{\code{cross}}{a numeric vector}
    \item{\code{self}}{a numeric vector}
    \item{\code{height}}{a numeric vector}
    \item{\code{method}}{a numeric vector}
    \item{\code{differ}}{a numeric vector}
    \item{\code{sgnrnks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Darwin)
attach(Darwin)
qqnorm(differ)
qqline(differ)
shapiro.test(differ)
wilcox.test(cross,self,paired=TRUE)
detach(Darwin)
}
\keyword{datasets}
