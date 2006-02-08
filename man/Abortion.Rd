\name{Abortion}
\alias{Abortion}
\docType{data}

\title{Abortion rate by region of country}

\description{Data use in Exercise 8.51}

\usage{Abortion}

\format{
  A data frame with 51 observations on the following 9 variables.
  \describe{
    \item{\code{state}}{a factor with levels \code{alabama} \code{alaska} \code{arizona} \code{arkansas} \code{california} \code{colorado} \code{connecticut} \code{delaware} \code{dist of columbia} \code{florida} \code{georgia} \code{hawaii} \code{idaho} \code{illinois} \code{indiana} \code{iowa} \code{kansas} \code{kentucky} \code{louisiana} \code{maine} \code{maryland} \code{massachusetts} \code{michigan} \code{minnesota} \code{mississippi} \code{missouri} \code{montana} \code{nebraska} \code{nevada} \code{new hampshire} \code{new jersey} \code{new mexico} \code{new york} \code{north carolina} \code{north dakota} \code{ohio} \code{oklahoma} \code{oregon} \code{pennsylvania} \code{rhode island} \code{south carolina} \code{south dakota} \code{tennessee} \code{texas} \code{utah} \code{vermont} \code{virginia} \code{washington} \code{west virginia} \code{wisconsin} \code{wyoming}}
    \item{\code{region}}{a factor with levels \code{midwest} \code{northeast} \code{south} \code{west}}
    \item{\code{regcode}}{a numeric vector}
    \item{\code{X88rate}}{a numeric vector}
    \item{\code{X92rate}}{a numeric vector}
    \item{\code{X96rate}}{a numeric vector}
    \item{\code{X88provid}}{a numeric vector}
    \item{\code{X92provid}}{a numeric vector}
    \item{\code{lowhigh}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
attach(Abortion)
AbortionRate <- cut(X96rate,breaks=c(0,20,10000) )
levels(AbortionRate) <- c("Low","High")
table(region,AbortionRate)
chisq.test(table(region,AbortionRate))
detach(Abortion)
}

\keyword{datasets}
