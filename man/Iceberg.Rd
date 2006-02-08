\name{Iceberg}
\alias{Iceberg}
\docType{data}
\title{Number of icebergs sighted each month south of Newfoundland and south of the Grand Banks in 1920}
\description{
  Data for Exercise 2.46 and 2.60
}
\usage{Iceberg}
\format{
  A data frame with 12 observations on the following 3 variables.
  \describe{
    \item{\code{Month}}{a factor with levels \code{Apr} \code{Aug} \code{Dec} \code{Feb} \code{Jan} \code{Jul} \code{Jun} \code{Mar} \code{May} \code{Nov} \code{Oct} \code{Sep}}
    \item{\code{Newfound}}{a numeric vector}
    \item{\code{GrandBk}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Iceberg)
attach(Iceberg)
plot(GrandBk,Newfound)
abline(lm(Newfound~GrandBk))
detach(Iceberg)
}
\keyword{datasets}
