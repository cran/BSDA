\name{Engineer}
\alias{Engineer}
\docType{data}
\title{Salaries after 10 years for graduates of three different universities}
\description{
  Data for Example 10.7
}
\usage{Engineer}
\format{
  A data frame with 51 observations on the following 6 variables.
  \describe{
    \item{\code{UnivA}}{a numeric vector}
    \item{\code{UnivB}}{a numeric vector}
    \item{\code{UnivC}}{a numeric vector}
    \item{\code{salary}}{a numeric vector}
    \item{\code{university}}{a numeric vector}
    \item{\code{ranks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Engineer)
attach(Engineer)
boxplot(salary~university)
kruskal.test(salary~as.factor(university))
detach(Engineer)
}
\keyword{datasets}
