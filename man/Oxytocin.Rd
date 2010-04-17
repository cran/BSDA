\name{Oxytocin}
\alias{Oxytocin}
\docType{data}
\title{Arterial blood pressure of 11 subjects before and after receiving oxytocin}
\description{
  Data for Exercise 7.86
}
\usage{Oxytocin}
\format{
  A data frame with 11 observations on the following 3 variables.
  \describe{
    \item{\code{Subject}}{a numeric vector}
    \item{\code{Before}}{a numeric vector}
    \item{\code{After}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Oxytocin)
attach(Oxytocin)
DIF <- Before - After
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
t.test(Before,After,paired=TRUE)
detach(Oxytocin)
}
\keyword{datasets}
