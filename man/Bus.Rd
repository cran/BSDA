\name{Bus}
\alias{Bus}
\docType{data}
\title{Attendance of bus drivers versus attendance}
\description{
  Data for Exercise 8.25
}
\usage{Bus}
\format{
  A data frame with 2 observations on the following 6 variables.
  \describe{
    \item{\code{Attend}}{a factor with levels \code{Absent} \code{Present}}
    \item{\code{AM}}{a numeric vector}
    \item{\code{Noon}}{a numeric vector}
    \item{\code{PM}}{a numeric vector}
    \item{\code{Swing}}{a numeric vector}
    \item{\code{Split}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Bus)
attach(Bus)
Bus
chisq.test(Bus[,2:6])
detach(Bus)
}
\keyword{datasets}
