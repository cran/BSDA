\name{Bypass}
\alias{Bypass}
\docType{data}
\title{ Median charges for coronary bypass at 17 hospitals in North Carolina}
\description{
  Data for Exercises 5.104 and 6.43 
}
\usage{Bypass}
\format{
  A data frame with 17 observations on the following 2 variables.
  \describe{
    \item{\code{hospital}}{a factor with levels \code{Carolinas Med Ct} \code{Duke Med Ct} \code{Durham Regional} \code{Forsyth Memorial} \code{Frye Regional} \code{High Point Regional} \code{Memorial Mission} \code{Mercy} \code{Moore Regional} \code{Moses Cone Memorial} \code{NC Baptist} \code{New Hanover Regional} \code{Pitt Co. Memorial} \code{Presbyterian} \code{Rex} \code{Univ of North Carolina} \code{Wake County}}
    \item{\code{charge}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Bypass)
attach(Bypass)
EDA(charge)
t.test(charge,conf.level=.90)$conf
t.test(charge,mu=35000)
detach(Bypass)
}
\keyword{datasets}
