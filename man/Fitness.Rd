\name{Fitness}
\alias{Fitness}
\docType{data}
\title{Number of sit-ups before and after a physical fitness course}
\description{
  Data for Exercise 7.71
}
\usage{Fitness}
\format{
  A data frame with 9 observations on the following 2 variables.
  \describe{
    \item{\code{Before}}{a numeric vector}
    \item{\code{After}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Fitness)
attach(Fitness)
DIF <- After - Before
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
t.test(After,Before,paired=TRUE,alternative="greater")
detach(Fitness)
}
\keyword{datasets}
