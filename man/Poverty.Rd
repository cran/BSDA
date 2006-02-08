\name{Poverty}
\alias{Poverty}
\docType{data}
\title{Percent poverty and crime rate for selected cities}
\description{
  Data for Exercise 9.11 and 9.17
}
\usage{Poverty}
\format{
  A data frame with 20 observations on the following 6 variables.
  \describe{
    \item{\code{City}}{a factor with levels \code{Atlanta} \code{Buffalo} \code{Cincinnati} \code{Cleveland} \code{Dayton, O} \code{Detroit} \code{Flint, Mich} \code{Fresno, C} \code{Gary, Ind} \code{Hartford, C} \code{Laredo} \code{Macon, Ga} \code{Miami} \code{Milwaukee} \code{New Orleans} \code{Newark, NJ} \code{Rochester,NY} \code{Shreveport} \code{St. Louis} \code{Waco, Tx}}
    \item{\code{Poverty}}{a numeric vector}
    \item{\code{Crime}}{a numeric vector}
    \item{\code{cindex}}{a numeric vector}
    \item{\code{popu}}{a numeric vector}
    \item{\code{ratio}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Poverty)
attach(Poverty)
plot(Crime,Poverty)
model <- lm(Poverty~Crime)
abline(model)
summary(model)
detach(Poverty)
remove(model)
}
\keyword{datasets}
