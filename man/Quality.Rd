\name{Quality}
\alias{Quality}
\docType{data}
\title{Quality control test scores on two manufacturing processes}
\description{
  Data for Exercise 7.81
}
\usage{Quality}
\format{
  A data frame with 8 observations on the following 2 variables.
  \describe{
    \item{\code{Process1}}{a numeric vector}
    \item{\code{Process2}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Quality)
attach(Quality)
qqnorm(Process1)
qqline(Process1)
shapiro.test(Process1)
qqnorm(Process2)
qqline(Process2)
shapiro.test(Process2)
t.test(Process1,Process2)
detach(Quality)
}
\keyword{datasets}
