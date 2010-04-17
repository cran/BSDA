\name{Movie}
\alias{Movie}
\docType{data}
\title{Moral attitude scale on 15 subjects before and after viewing a movie}
\description{
  Data for Exercise 7.53
}
\usage{Movie}
\format{
  A data frame with 12 observations on the following 3 variables.
  \describe{
    \item{\code{Before}}{a numeric vector}
    \item{\code{After}}{a numeric vector}
    \item{\code{differ}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Movie)
attach(Movie)
qqnorm(differ)
qqline(differ)
shapiro.test(differ)
t.test(After,Before,paired=TRUE,conf.level=.99)
wilcox.test(After,Before,paired=TRUE)
detach(Movie)
}
\keyword{datasets}
