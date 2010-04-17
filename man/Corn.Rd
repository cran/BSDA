\name{Corn}
\alias{Corn}
\docType{data}
\title{ Comparison of the yields of a new variety and a standard variety of corn planted on 12 plots of land}
\description{
  Data for Exercise 7.77
}
\usage{Corn}
\format{
  A data frame with 12 observations on the following 3 variables.
  \describe{
    \item{\code{New}}{a numeric vector}
    \item{\code{Standard}}{a numeric vector}
    \item{\code{differ}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Corn)
attach(Corn)
boxplot(differ)
qqnorm(differ)
qqline(differ)
shapiro.test(differ)
t.test(New,Standard,paired=TRUE,alternative="greater")
detach(Corn)
}
\keyword{datasets}
