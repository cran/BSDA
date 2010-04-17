\name{Florida2000}
\alias{Florida2000}
\docType{data}
\title{Florida voter results in the 2000 presidential election}
\description{
  Data for Statistical Insight Chapter 2
}
\usage{Florida2000}
\format{
  A data frame with 67 observations on the following 12 variables.
  \describe{
    \item{\code{County}}{a factor with levels \code{ALACHUA} \code{BAKER} \code{BAY} \code{BRADFORD} \code{BREVARD} \code{BROWARD} \code{CALHOUN} \code{CHARLOTTE} \code{CITRUS} \code{CLAY} \code{COLLIER} \code{COLUMBIA} \code{DADE} \code{DE SOTO} \code{DIXIE} \code{DUVAL} \code{ESCAMBIA} \code{FLAGLER} \code{FRANKLIN} \code{GADSDEN} \code{GILCHRIST} \code{GLADES} \code{GULF} \code{HAMILTON} \code{HARDEE} \code{HENDRY} \code{HERNANDO} \code{HIGHLANDS} \code{HILLSBOROUGH} \code{HOLMES} \code{INDIAN RIVER} \code{JACKSON} \code{JEFFERSON} \code{LAFAYETTE} \code{LAKE} \code{LEE} \code{LEON} \code{LEVY} \code{LIBERTY} \code{MADISON} \code{MANATEE} \code{MARION} \code{MARTIN} \code{MONROE} \code{NASSAU} \code{OKALOOSA} \code{OKEECHOBEE} \code{ORANGE} \code{OSCEOLA} \code{PALM BEACH} \code{PASCO} \code{PINELLAS} \code{POLK} \code{PUTNAM} \code{SANTA ROSA} \code{SARASOTA} \code{SEMINOLE} \code{ST. JOHNS} \code{ST. LUCIE} \code{SUMTER} \code{SUWANNEE} \code{TAYLOR} \code{UNION} \code{VOLUSIA} \code{WAKULLA} \code{WALTON} \code{WASHINGTON}}
    \item{\code{GORE}}{a numeric vector}
    \item{\code{BUSH}}{a numeric vector}
    \item{\code{BUCHANAN}}{a numeric vector}
    \item{\code{NADER}}{a numeric vector}
    \item{\code{BROWNE}}{a numeric vector}
    \item{\code{HAGELIN}}{a numeric vector}
    \item{\code{HARRIS}}{a numeric vector}
    \item{\code{MCREYNOLDS}}{a numeric vector}
    \item{\code{MOOREHEAD}}{a numeric vector}
    \item{\code{PHILLIPS}}{a numeric vector}
    \item{\code{Total}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Florida2000)
attach(Florida2000)
plot(Total,BUCHANAN,xlab="Total votes cast (in thousands)",
ylab="Votes for Buchanan")
detach(Florida2000)
}
\keyword{datasets}
