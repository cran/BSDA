\name{Apolipop}
\alias{Apolipop}
\docType{data}

\title{Level of apolipoprotein B and number of cups of coffee consumed per day for 15 adult males}

\description{Data used in Examples 9.2 and 9.9}

\usage{Apolipop}

\format{
  A data frame with 15 observations on the following 4 variables.
  \describe{
    \item{\code{coffee}}{a numeric vector}
    \item{\code{apolipB}}{a numeric vector}
    \item{\code{SRES1}}{a numeric vector}
    \item{\code{FITS1}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
attach(Apolipop)
str(Apolipop)
plot(coffee,apolipB)
linmod <- lm(apolipB~coffee)
summary(linmod)
# plot(linmod)
detach(Apolipop)
}

\keyword{datasets}
