\name{Spelling}
\alias{Spelling}
\docType{data}
\title{ Spelling scores for 9 eighth graders before and after a 2-week course of instruction}
\description{
  Data for Exercise 7.56
}
\usage{Spelling}
\format{
  A data frame with 9 observations on the following 3 variables.
  \describe{
    \item{\code{Before}}{a numeric vector}
    \item{\code{After}}{a numeric vector}
    \item{\code{differ}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Spelling)
attach(Spelling)
qqnorm(differ)
qqline(differ)
shapiro.test(differ)
t.test(After,Before,paired=TRUE,alternative="greater")
detach(Spelling)
}
\keyword{datasets}
