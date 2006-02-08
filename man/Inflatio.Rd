\name{Inflatio}
\alias{Inflatio}
\docType{data}
\title{Private pay increase of salaried employees versus inflation rate}
\description{
  Data for Exercises 2.12 and 2.29
}
\usage{Inflatio}
\format{
  A data frame with 24 observations on the following 5 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{pay}}{a numeric vector}
    \item{\code{increase}}{a numeric vector}
    \item{\code{inflation}}{a numeric vector}
    \item{\code{C6.T}}{a factor with levels \code{alow} \code{bmiddle} \code{high}}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Inflatio)
attach(Inflatio)
plot(inflation,increase)
cor(inflation,increase,use="complete.obs")
detach(Inflatio)
}
\keyword{datasets}
