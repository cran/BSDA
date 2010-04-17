\name{Chesapea}
\alias{Chesapea}
\docType{data}
\title{Surface salinity measurements taken offshore from Annapolis, Maryland in 1927}
\description{
  Data for Exercise 6.46
}
\usage{Chesapea}
\format{
  A data frame with 16 observations on the following variable.
  \describe{
    \item{\code{salinity}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Chesapea)
attach(Chesapea)
qqnorm(salinity)
qqline(salinity)
shapiro.test(salinity)
t.test(salinity,mu=7)
detach(Chesapea)
}
\keyword{datasets}
