\name{Habits}
\alias{Habits}
\docType{data}
\title{Study habits of students in two matched school districts}
\description{
  Data for Exercise 7.57
}
\usage{Habits}
\format{
  A data frame with 11 observations on the following 4 variables.
  \describe{
    \item{\code{A}}{a numeric vector}
    \item{\code{B}}{a numeric vector}
    \item{\code{differ}}{a numeric vector}
    \item{\code{signrks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Habits)
attach(Habits)
qqnorm(differ)
qqline(differ)
shapiro.test(differ)
t.test(B,A,paired=TRUE,alternative="less")
wilcox.test(B,A,paired=TRUE,alternative="less")
detach(Habits)
}
\keyword{datasets}
