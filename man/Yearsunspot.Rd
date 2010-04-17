\name{Yearsunspot}
\alias{Yearsunspot}
\docType{data}
\title{ Monthly sunspot activity from 1974 to 2000}
\description{Data for Exercise 2.7}
\usage{Yearsunspot}
\format{
  A data frame with 252 observations on the following 24 variables.
  \describe{
    \item{\code{X1979}}{a numeric vector}
    \item{\code{X1980}}{a numeric vector}
    \item{\code{X1981}}{a numeric vector}
    \item{\code{X1982}}{a numeric vector}
    \item{\code{X1983}}{a numeric vector}
    \item{\code{X1984}}{a numeric vector}
    \item{\code{X1985}}{a numeric vector}
    \item{\code{X1986}}{a numeric vector}
    \item{\code{X1987}}{a numeric vector}
    \item{\code{X1988}}{a numeric vector}
    \item{\code{X1989}}{a numeric vector}
    \item{\code{X1990}}{a numeric vector}
    \item{\code{X1991}}{a numeric vector}
    \item{\code{X1992}}{a numeric vector}
    \item{\code{X1993}}{a numeric vector}
    \item{\code{X1994}}{a numeric vector}
    \item{\code{X1995}}{a numeric vector}
    \item{\code{X1996}}{a numeric vector}
    \item{\code{X1997}}{a numeric vector}
    \item{\code{X1998}}{a numeric vector}
    \item{\code{X1999}}{a numeric vector}
    \item{\code{X2000}}{a numeric vector}
    \item{\code{SSN}}{a numeric vector}
    \item{\code{year}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Yearsunspot)
attach(Yearsunspot)
boxplot(SSN~year,main="Exercise 2.7",col="lightblue")
detach(Yearsunspot)
}
\keyword{datasets}
