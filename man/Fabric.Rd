\name{Fabric}
\alias{Fabric}
\docType{data}
\title{Measures of softness of 10 different clothing garments washed with and without a softener}
\description{
  Data for Exercise 7.21
}
\usage{Fabric}
\format{
  A data frame with 10 observations on the following 3 variables.
  \describe{
    \item{\code{Type}}{a numeric vector}
    \item{\code{With}}{a numeric vector}
    \item{\code{Without}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Fabric)
attach(Fabric)
DIF <- With - Without
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
wilcox.test(With,Without,paired=TRUE,alternative="greater")
detach(Fabric)
remove(DIF)
}
\keyword{datasets}
