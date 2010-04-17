\name{Stress}
\alias{Stress}
\docType{data}
\title{ Short-term memory test scores on 12 subjects before and after a stressful situation}
\description{
  Data for Example 7.20
}
\usage{Stress}
\format{
  A data frame with 12 observations on the following 2 variables.
  \describe{
    \item{\code{Prestre}}{a numeric vector}
    \item{\code{Poststre}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Stress)
attach(Stress)
DIF <- Poststre -Prestre
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
t.test(Poststre,Prestre,paired=TRUE,alternative="less")
detach(Stress)
remove(DIF)
}
\keyword{datasets}
