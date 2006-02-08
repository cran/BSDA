\name{Domestic}
\alias{Domestic}
\docType{data}
\title{Rates of domestic violence per 1,000 women by age groups}
\description{
  Data for Exercise 1.20
}
\usage{Domestic}
\format{
  A data frame with 5 observations on the following 2 variables.
  \describe{
    \item{\code{Age}}{a factor with levels \code{12-19} \code{20-24} \code{25-34} \code{35-49} \code{50-64}}
    \item{\code{Rate}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Domestic)
attach(Domestic)
names(Rate) <- Age
barplot(Rate,col="gold")
pie(Rate)
detach(Domestic)
}
\keyword{datasets}
