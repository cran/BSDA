\name{Tablrock}
\alias{Tablrock}
\docType{data}
\title{ Ozone concentrations at Mt. Mitchell, North Carolina}
\description{
  Data for Exercise 5.63
}
\usage{Tablrock}
\format{
  A data frame with 719 observations on the following 16 variables.
  \describe{
    \item{\code{hour}}{a factor with levels \code{00:00} \code{01:00} \code{02:00} \code{03:00} \code{04:00} \code{05:00} \code{06:00} \code{07:00} \code{08:00} \code{09:00} \code{10:00} \code{11:00} \code{12:00} \code{13:00} \code{14:00} \code{15:00} \code{16:00} \code{17:00} \code{18:00} \code{19:00} \code{20:00} \code{21:00} \code{22:00} \code{23:00}}
    \item{\code{X03}}{a numeric vector}
    \item{\code{tmp}}{a numeric vector}
    \item{\code{vdc}}{a numeric vector}
    \item{\code{wd}}{a numeric vector}
    \item{\code{ws}}{a numeric vector}
    \item{\code{amb}}{a numeric vector}
    \item{\code{dew}}{a numeric vector}
    \item{\code{so2}}{a numeric vector}
    \item{\code{no}}{a numeric vector}
    \item{\code{no2}}{a numeric vector}
    \item{\code{nox}}{a numeric vector}
    \item{\code{co}}{a numeric vector}
    \item{\code{co2}}{a numeric vector}
    \item{\code{gas}}{a numeric vector}
    \item{\code{air}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Tablrock)
attach(Tablrock)
EDA(X03)
t.test(X03,conf.level=.99)$conf
detach(Tablrock)
}
\keyword{datasets}
