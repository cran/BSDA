\name{Hurrican}
\alias{Hurrican}
\docType{data}
\title{Number of storms, hurricanes and El Nino effects from 1950 through 1995}
\description{
  Data for Exercises 1.38, 10.19, and Example 1.6
}
\usage{Hurrican}
\format{
  A data frame with 46 observations on the following 5 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{storms}}{a numeric vector}
    \item{\code{hurrican}}{a numeric vector}
    \item{\code{ElNino}}{a factor with levels \code{cold} \code{neutral} \code{warm}}
    \item{\code{code}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Hurrican)
attach(Hurrican)    
barplot(table(hurrican),col="blue",main="Problem 1.38",
xlab="Number of Hurricanes",ylab="Number of Seasons")
boxplot(storms~ElNino)
anova(lm(storms~ElNino))
detach(Hurrican)
}
\keyword{datasets}
