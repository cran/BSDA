\name{Rat}
\alias{Rat}
\docType{data}
\title{Survival times of 20 rats exposed to high levels of radiation}
\description{
  Data for Exercise 1.52, 1.76, 5.62, and 6.44
}
\usage{Rat}
\format{
  A data frame with 20 observations on the following variable.
  \describe{
    \item{\code{survival.time}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Rat)
attach(Rat)
hist(survival.time)
qqnorm(survival.time,col="blue")
qqline(survival.time,col="red")
t.test(survival.time)$conf
t.test(survival.time,mu=100,alternative="greater")
detach(Rat)
}
\keyword{datasets}
