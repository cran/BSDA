\name{Coaxial}
\alias{Coaxial}
\docType{data}
\title{Signal loss from three types of coxial cable}
\description{
  Data for Exercise 10.24 and 10.25
}
\usage{Coaxial}
\format{
  A data frame with 45 observations on the following 5 variables.
  \describe{
    \item{\code{Type.A}}{a numeric vector}
    \item{\code{Type.B}}{a numeric vector}
    \item{\code{Type.C}}{a numeric vector}
    \item{\code{Signal}}{a numeric vector}
    \item{\code{Cable}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Coaxial)
attach(Coaxial)
boxplot(Signal~Cable)
kruskal.test(Signal~as.factor(Cable))
detach(Coaxial)
}
\keyword{datasets}
