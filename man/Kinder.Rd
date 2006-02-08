\name{Kinder}
\alias{Kinder}
\docType{data}
\title{ Reading scores for first grade children who attended kindergarten versus those who did not}
\description{
  Data for Exercise 7.68
}
\usage{Kinder}
\format{
  A data frame with 8 observations on the following 3 variables.
  \describe{
    \item{\code{Pair}}{a numeric vector}
    \item{\code{Kinder}}{a numeric vector}
    \item{\code{NoKinder}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Kinder)
attach(Kinder)
DIF <- Kinder - NoKinder
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
t.test(Kinder, NoKinder,paired=TRUE,alternative="greater")
detach(Kinder)
remove(DIF)
}
\keyword{datasets}
