\name{Lodge}
\alias{Lodge}
\docType{data}
\title{Measured traffic at three prospective locations for a motor lodge}
\description{
  Data for Exercise 10.33
}
\usage{Lodge}
\format{
  A data frame with 45 observations on the following 6 variables.
  \describe{
    \item{\code{SiteA}}{a numeric vector}
    \item{\code{SiteB}}{a numeric vector}
    \item{\code{SiteC}}{a numeric vector}
    \item{\code{Traffic}}{a numeric vector}
    \item{\code{Site}}{a numeric vector}
    \item{\code{Ranks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Lodge)
attach(Lodge)
boxplot(Traffic~Site)
anova(lm(Traffic~as.factor(Site)))
detach(Lodge)
}
\keyword{datasets}
