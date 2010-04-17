\name{Longtail}
\alias{Longtail}
\docType{data}
\title{Long-tailed distributions to illustrate Kruskal Wallis test}
\description{
  Data for Exercise 10.45
}
\usage{Longtail}
\format{
  A data frame with 60 observations on the following 6 variables.
  \describe{
    \item{\code{GroupA}}{a numeric vector}
    \item{\code{GroupB}}{a numeric vector}
    \item{\code{GroupC}}{a numeric vector}
    \item{\code{score}}{a numeric vector}
    \item{\code{Group}}{a numeric vector}
    \item{\code{Ranks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Longtail)
attach(Longtail)
boxplot(score~Group)
kruskal.test(score~as.factor(Group))
anova(lm(score~as.factor(Group)))
detach(Longtail)
}
\keyword{datasets}
