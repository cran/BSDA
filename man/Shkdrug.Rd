\name{Shkdrug}
\alias{Shkdrug}
\docType{data}
\title{Effects of a drug and electroshock therapy on the ability to solve simple tasks}
\description{
  Data for Exercises 10.11 and 10.12
}
\usage{Shkdrug}
\format{
  A data frame with 64 observations on the following 6 variables.
  \describe{
    \item{\code{Drug.Shk}}{a numeric vector}
    \item{\code{Drug.NoS}}{a numeric vector}
    \item{\code{NoDrug.S}}{a numeric vector}
    \item{\code{NoDg.NoS}}{a numeric vector}
    \item{\code{Treatment}}{a factor with levels \code{Drug/NoS} \code{Drug/Shk} \code{NoDg/NoS} \code{NoDrug/S}}
    \item{\code{Response}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Shkdrug)
attach(Shkdrug)
boxplot(Response~Treatment)
anova(lm(Response~Treatment))
detach(Shkdrug)
}
\keyword{datasets}
