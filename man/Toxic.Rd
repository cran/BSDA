\name{Toxic}
\alias{Toxic}
\docType{data}
\title{ Hazardous waste sites near minority communities}
\description{
  Data for Exercises 1.55, 5.08, 5.109, 8.58, and 10.35
}
\usage{Toxic}
\format{
  A data frame with 51 observations on the following 5 variables.
  \describe{
    \item{\code{state}}{a factor with levels \code{alabama} \code{alaska} \code{arizona} \code{arkansas} \code{california} \code{colorado} \code{connecticut} \code{delaware} \code{dist of columbia} \code{florida} \code{georgia} \code{hawaii} \code{idaho} \code{illinois} \code{indiana} \code{iowa} \code{kansas} \code{kentucky} \code{louisiana} \code{maine} \code{maryland} \code{massachusetts} \code{michigan} \code{minnesota} \code{mississippi} \code{missouri} \code{montana} \code{nebraska} \code{nevada} \code{new hampshire} \code{new jersey} \code{new mexico} \code{new york} \code{north carolina} \code{north dakota} \code{ohio} \code{oklahoma} \code{oregon} \code{pennsylvania} \code{rhode island} \code{south carolina} \code{south dakota} \code{tennessee} \code{texas} \code{utah} \code{vermont} \code{virginia} \code{washington} \code{west virginia} \code{wisconsin} \code{wyoming}}
    \item{\code{region}}{a factor with levels \code{midwest} \code{northeast} \code{south} \code{west}}
    \item{\code{sites}}{a numeric vector}
    \item{\code{minority}}{a numeric vector}
    \item{\code{percent}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Toxic)
attach(Toxic)
hist(minority,prob=TRUE)
lines(density(minority))
SIGN.test(sites,conf.level=.98)
boxplot(sites~region)
kruskal.test(sites~as.factor(region))
detach(Toxic)
}
\keyword{datasets}
