\name{Vaccine}
\alias{Vaccine}
\docType{data}
\title{Reported serious reactions due to vaccines in 11 southern states}
\description{
  Data for Exercise 1.111
}
\usage{Vaccine}
\format{
  A data frame with 11 observations on the following 2 variables.
  \describe{
    \item{\code{State}}{a factor with levels \code{Alabama} \code{Arkansas} \code{Florida} \code{Georgia} \code{Louisiana} \code{Mississippi} \code{North Carolina} \code{Oklahoma} \code{South Carolina} \code{Tennessee} \code{Texas}}
    \item{\code{number}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Vaccine)
attach(Vaccine)
fn <- fivenum(number)
fn
iqr <- IQR(number)
ll <- fn[2]-1.5*iqr
ul <- fn[4]+1.5*iqr
limits <- c(ll,ul)
limits
boxplot(number)
detach(Vaccine)
}
\keyword{datasets}
