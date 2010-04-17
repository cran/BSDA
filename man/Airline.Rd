\name{Airline}
\alias{Airline}
\docType{data}

\title{Percentage of on-time arrivals and number of complaints for 11 airlines}

\description{Data for Exercise 2.9}

\usage{Airline}

\format{
  A data frame with 11 observations on the following 3 variables.
  \describe{
    \item{\code{airline}}{a factor with levels \code{Alaska} \code{Amer West} \code{American} \code{Continental} \code{Delta} \code{Northwest} \code{Pan Am} \code{Southwest} \code{TWA} \code{United} \code{USAir}}
    \item{\code{ontime}}{a numeric vector}
    \item{\code{complnt}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Airline)
attach(Airline)
barplot(complnt,names.arg=airline,col="lightblue")
plot(ontime,complnt)
detach(Airline)
}

\keyword{datasets}
