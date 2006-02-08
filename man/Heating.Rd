\name{Heating}
\alias{Heating}
\docType{data}
\title{Fuel efficiency ratings for three types of oil heaters}
\description{
  Data for Exercise 10.32
}
\usage{Heating}
\format{
  A data frame with 30 observations on the following 6 variables.
  \describe{
    \item{\code{TypeA}}{a numeric vector}
    \item{\code{TypeB}}{a numeric vector}
    \item{\code{TypeC}}{a numeric vector}
    \item{\code{Rating}}{a numeric vector}
    \item{\code{Type}}{a numeric vector}
    \item{\code{Ranks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Heating)
attach(Heating)
boxplot(Rating~Type)
kruskal.test(Rating~as.factor(Type))
detach(Heating)
}
\keyword{datasets}
