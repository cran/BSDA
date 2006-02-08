\name{Absent}
\alias{Absent}
\docType{data}

\title{Number of absent days for 20 employees}

\description{ Data used in Exercise 1.28}

\usage{Absent}

\format{
  A data frame with 20 observations on the following 4 variables.
  \describe{
    \item{\code{days}}{a numeric vector}
    \item{\code{days_1}}{a numeric vector}
    \item{\code{Count}}{a numeric vector}
    \item{\code{Percent}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Absent)
attach(Absent)
table(days)
barplot(table(days),col="pink")
detach(Absent)
}
\keyword{datasets}
