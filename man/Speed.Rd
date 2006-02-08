\name{Speed}
\alias{Speed}
\docType{data}
\title{Speed reading scores before and after a course on speed reading}
\description{
  Data for Exercise 7.58
}
\usage{Speed}
\format{
  A data frame with 15 observations on the following 4 variables.
  \describe{
    \item{\code{Before}}{a numeric vector}
    \item{\code{After}}{a numeric vector}
    \item{\code{differ}}{a numeric vector}
    \item{\code{signrnks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Speed)
attach(Speed)
qqnorm(differ)
qqline(differ)
shapiro.test(differ)
t.test(After,Before,paired=TRUE,alternative="greater")
wilcox.test(After,Before,paired=TRUE,alternative="greater")
detach(Speed)
}
\keyword{datasets}
