\name{Achieve}
\alias{Achieve}
\docType{data}

\title{Math achievement test scores by gender for 25 high school students}

\description{Data used in Example 7.14 and Exercise 10.7}

\usage{Achieve}

\format{
  A data frame with 25 observations on the following 4 variables.
  \describe{
    \item{\code{Score}}{a numeric vector}
    \item{\code{Gender}}{a numeric vector}
    \item{\code{Female}}{a numeric vector}
    \item{\code{Male}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Achieve)
attach(Achieve)
anova(lm(Score~Gender))
t.test(Female,Male,var.equal=TRUE)
detach(Achieve)
}

\keyword{datasets}
