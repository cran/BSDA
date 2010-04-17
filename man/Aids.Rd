\name{Aids}
\alias{Aids}
\docType{data}

\title{Incubation times for 295 patients thought to be infected with HIV by a blood transfusion}

\description{Data used in Exercise 6.60}

\usage{Aids}

\format{
  A data frame with 295 observations on the following 11 variables.
  \describe{
    \item{\code{duration}}{a numeric vector}
    \item{\code{age}}{a numeric vector}
    \item{\code{group}}{a numeric vector}
    \item{\code{duratio1}}{a numeric vector}
    \item{\code{children}}{a numeric vector}
    \item{\code{duratio2}}{a numeric vector}
    \item{\code{adults}}{a numeric vector}
    \item{\code{duratio3}}{a numeric vector}
    \item{\code{elderly}}{a numeric vector}
    \item{\code{SRES1}}{a numeric vector}
    \item{\code{FITS1}}{a numeric vector}
  }
}


\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Aids)
attach(Aids)
EDA(duration)
t.test(duration,mu=30,alternative="greater")
SIGN.test(duration,md=24,alternative="greater")
detach(Aids)
}

\keyword{datasets}
