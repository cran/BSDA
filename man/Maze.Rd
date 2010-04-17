\name{Maze}
\alias{Maze}
\docType{data}
\title{Error scores for four groups of experimental animals running a maze}
\description{
  Data for Exercise 10.13
}
\usage{Maze}
\format{
  A data frame with 32 observations on the following 6 variables.
  \describe{
    \item{\code{CondA}}{a numeric vector}
    \item{\code{CondB}}{a numeric vector}
    \item{\code{CondC}}{a numeric vector}
    \item{\code{CondD}}{a numeric vector}
    \item{\code{score}}{a numeric vector}
    \item{\code{condition}}{a factor with levels \code{CondA} \code{CondB} \code{CondC} \code{CondD}}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Maze)
attach(Maze)
boxplot(score~condition)
anova(lm(score~condition))
detach(Maze)
}
\keyword{datasets}
