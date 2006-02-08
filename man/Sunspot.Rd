\name{Sunspot}
\alias{Sunspot}
\docType{data}
\title{Wolfer sunspot numbers from 1700 through 2000}
\description{
  Data for Example 1.7
}
\usage{Sunspot}
\format{
  A data frame with 301 observations on the following 2 variables.
  \describe{
    \item{\code{year}}{a numeric vector}
    \item{\code{sunspots}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Sunspot)
attach(Sunspot)
plot(year,sunspots,type="l",main="Yearly Sunspots")    # Using standard plot
library(lattice)
xyplot(sunspots ~ 1700:2000, xlab = "", type = "l",main="Yearly Sunspots")
xyplot(sunspots ~ 1700:2000, xlab = "", type = "l", aspect="xy",
main="Yearly Sunspots")
detach(Sunspot)
}
\keyword{datasets}
