\name{Phone}
\alias{Phone}
\docType{data}
\title{Length of long-distance phone calls for a small business firm}
\description{
  Data for Exercise 6.95
}
\usage{Phone}
\format{
  A data frame with 20 observations on the following variable.
  \describe{
    \item{\code{time}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Phone)
attach(Phone)
qqnorm(time)
qqline(time)
shapiro.test(time)
SIGN.test(time,md=5,alternative="greater")
detach(Phone)
}
\keyword{datasets}
