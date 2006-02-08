\name{Epaminicompact}
\alias{Epaminicompact}
\docType{data}
\title{Fuel efficiency ratings for compact vehicles in 2001}
\description{
  Data for Exercise 1.65
}
\usage{Epaminicompact}
\format{
  A data frame with 22 observations on the following 10 variables.
  \describe{
    \item{\code{Class}}{a factor with levels \code{MINICOMPACT CARS}}
    \item{\code{Manufacturer}}{a factor with levels \code{AUDI} \code{BMW} \code{JAGUAR} \code{MERCEDES-BENZ} \code{MITSUBISHI} \code{PORSCHE}}
    \item{\code{carline.name}}{a factor with levels \code{325CI CONVERTIBLE} \code{330CI CONVERTIBLE} \code{911 CARRERA 2/4} \code{911 TURBO} \code{CLK320 (CABRIOLET)} \code{CLK430 (CABRIOLET)} \code{ECLIPSE SPYDER} \code{JAGUAR XK8 CONVERTIBLE} \code{JAGUAR XKR CONVERTIBLE} \code{M3 CONVERTIBLE} \code{TT COUPE} \code{TT COUPE QUATTRO}}
    \item{\code{displ}}{a numeric vector}
    \item{\code{cyl}}{a numeric vector}
    \item{\code{trans}}{a factor with levels \code{Auto(L5)} \code{Auto(S4)} \code{Auto(S5)} \code{Manual(M5)} \code{Manual(M6)}}
    \item{\code{drv}}{a factor with levels \code{4} \code{F} \code{R}}
    \item{\code{cty}}{a numeric vector}
    \item{\code{hwy}}{a numeric vector}
    \item{\code{cmb}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Epaminicompact)
attach(Epaminicompact)
summary(cty)
detach(Epaminicompact)
}
\keyword{datasets}
