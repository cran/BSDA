\name{Cabinets}
\alias{Cabinets}
\docType{data}
\title{Estimates of costs of kitchen cabinets by two suppliers on 20 prospective homes}
\description{
  Data for Exercise 7.83
}
\usage{Cabinets}
\format{
  A data frame with 20 observations on the following 3 variables.
  \describe{
    \item{\code{Home}}{a numeric vector}
    \item{\code{SupplA}}{a numeric vector}
    \item{\code{SupplB}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Cabinets)
attach(Cabinets)
DIF <- SupplA - SupplB
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
t.test(SupplA,SupplB,paired=TRUE)
wilcox.test(SupplA,SupplB,paired=TRUE)
detach(Cabinets)
remove(DIF)
}
\keyword{datasets}
