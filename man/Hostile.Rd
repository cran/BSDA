\name{Hostile}
\alias{Hostile}
\docType{data}
\title{Hostility levels of high school students from rural, suburban, and urban areas}
\description{
  Data for Example 10.6
}
\usage{Hostile}
\format{
  A data frame with 45 observations on the following 6 variables.
  \describe{
    \item{\code{Rural}}{a numeric vector}
    \item{\code{Suburban}}{a numeric vector}
    \item{\code{Urban}}{a numeric vector}
    \item{\code{HLT}}{a numeric vector}
    \item{\code{Type}}{a numeric vector}
    \item{\code{Ranks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Hostile)
attach(Hostile)
boxplot(HLT~Type)
kruskal.test(HLT~as.factor(Type))
detach(Hostile)
}
\keyword{datasets}
