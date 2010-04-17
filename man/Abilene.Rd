\name{Abilene}
\alias{Abilene}
\docType{data}

\title{Crimes reported in Abilene, Texas}

\description{Data used in Exercise 1.23 and 2.79}

\usage{Abilene}
\format{
  A data frame with 8 observations on the following 5 variables.
  \describe{
    \item{\code{Crime}}{a factor with levels \code{Aggravated assault} \code{Arson} \code{Burglary} \code{Forcible rape} \code{Larceny theft} \code{Murder} \code{Robbery} \code{Vehicle theft}}
    \item{\code{X1992}}{a numeric vector}
    \item{\code{X92percent}}{a numeric vector}
    \item{\code{X1999}}{a numeric vector}
    \item{\code{X99percent}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
data(Abilene)
attach(Abilene)
par(mfrow=c(2,1))
barplot(X1992,names.arg=c("Murder","Rape","Robbery","Assault","Burglary",
"Larceny","V.Theft","Arson"),col="blue",main="Crime 1992")
barplot(X1999,names.arg=c("Murder","Rape","Robbery","Assault","Burglary",
"Larceny","V.Theft","Arson"),col="red",main="Crime 1999")
par(mfrow=c(1,1))
detach(Abilene)
}

\keyword{datasets}
