\name{Sat}
\alias{Sat}
\docType{data}
\title{SAT scores, percent taking exam and state funding per student by state for 1994, 1995 and 1999}
\description{
  Data for Statistical Insight Chapter 9
}
\usage{data(Sat)}
\format{
  A data frame with 51 observations on the following 16 variables.
  \describe{
    \item{\code{state}}{a factor with levels \code{alabama} \code{alaska} \code{arizona} \code{arkansas} \code{california} \code{colorado} \code{connecticut} \code{delaware} \code{dist of columbia} \code{florida} \code{georgia} \code{hawaii} \code{idaho} \code{illinois} \code{indiana} \code{iowa} \code{kansas} \code{kentucky} \code{louisiana} \code{maine} \code{maryland} \code{massachusetts} \code{michigan} \code{minnesota} \code{mississippi} \code{missouri} \code{montana} \code{nebraska} \code{nevada} \code{new hampshire} \code{new jersey} \code{new mexico} \code{new york} \code{north carolina} \code{north dakota} \code{ohio} \code{oklahoma} \code{oregon} \code{pennsylvania} \code{rhode island} \code{south carolina} \code{south dakota} \code{tennessee} \code{texas} \code{utah} \code{vermont} \code{virginia} \code{washington} \code{west virginia} \code{wisconsin} \code{wyoming}}
    \item{\code{verbal94}}{a numeric vector}
    \item{\code{math94}}{a numeric vector}
    \item{\code{total94}}{a numeric vector}
    \item{\code{percent94}}{a numeric vector}
    \item{\code{code94}}{a numeric vector}
    \item{\code{expend94}}{a numeric vector}
    \item{\code{verbal95}}{a numeric vector}
    \item{\code{math95}}{a numeric vector}
    \item{\code{total95}}{a numeric vector}
    \item{\code{verbal99}}{a numeric vector}
    \item{\code{math99}}{a numeric vector}
    \item{\code{total99}}{a numeric vector}
    \item{\code{percent99}}{a numeric vector}
    \item{\code{code99}}{a numeric vector}
    \item{\code{expend99}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Sat)
attach(Sat)
pairs(Sat)
detach(Sat)
}
\keyword{datasets}
