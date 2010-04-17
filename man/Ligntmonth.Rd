\name{Ligntmonth}
\alias{Ligntmonth}
\docType{data}
\title{Relationship between damage reports and deaths caused by lightning}
\description{
  Data for Exercise 2.6
}
\usage{Ligntmonth}
\format{
  A data frame with 12 observations on the following 4 variables.
  \describe{
    \item{\code{Month}}{a factor with levels \code{1/01/2000} \code{10/01/2000} \code{11/01/2000} \code{12/01/2000} \code{2/01/2000} \code{3/01/2000} \code{4/01/2000} \code{5/01/2000} \code{6/01/2000} \code{7/01/2000} \code{8/01/2000} \code{9/01/2000}}
    \item{\code{deaths}}{a numeric vector}
    \item{\code{injuries}}{a numeric vector}
    \item{\code{damage}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Ligntmonth)
attach(Ligntmonth)
plot(damage,deaths)
detach(Ligntmonth)
}
\keyword{datasets}
