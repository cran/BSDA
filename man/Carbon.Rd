\name{Carbon}
\alias{Carbon}
\docType{data}
\title{Carbon monoxide level measured at three industrial sites}
\description{
  Data for Exercise 10.28 and 10.29
}
\usage{Carbon}
\format{
  A data frame with 24 observations on the following 6 variables.
  \describe{
    \item{\code{SiteA}}{a numeric vector}
    \item{\code{SiteB}}{a numeric vector}
    \item{\code{SiteC}}{a numeric vector}
    \item{\code{monoxide}}{a numeric vector}
    \item{\code{Site}}{a numeric vector}
    \item{\code{Ranks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Carbon)
attach(Carbon)
boxplot(monoxide~Site)
kruskal.test(monoxide~as.factor(Site))
detach(Carbon)
}
\keyword{datasets}
