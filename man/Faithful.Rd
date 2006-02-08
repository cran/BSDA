\name{Faithful}
\alias{Faithful}
\docType{data}
\title{Waiting times between successive eruptions of the Old Faithful geyser}
\description{
  Data for Exercise 5.12 and 5.111
}
\usage{Faithful}
\format{
  A data frame with 299 observations on the following 2 variables.
  \describe{
    \item{\code{Time}}{a numeric vector}
    \item{\code{Eruption}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Faithful)
attach(Faithful)
hist(Time,prob=TRUE,xlab="Waiting time between eruptions",col="tomato")
lines(density(Time),col="red",lwd=3)
t.test(Time)$conf
detach(Faithful)
}
\keyword{datasets}
