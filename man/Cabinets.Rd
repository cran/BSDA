% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Cabinets}
\alias{Cabinets}
\title{Estimates of costs of kitchen cabinets by two suppliers on 20 prospective
homes}
\format{
A data frame/tibble with 20 observations on three variables
\describe{ 
\item{home}{a numeric vector} 
\item{supplA}{estimate for kitchen cabinets from supplier A (in dollars)} 
\item{supplB}{estimate for kitchen cabinets from supplier A (in dollars)} 
}
}
\usage{
Cabinets
}
\description{
Data for Exercise 7.83
}
\examples{

DIF <- Cabinets$supplA - Cabinets$supplB
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
with(data = Cabinets, 
     t.test(supplA, supplB, paired = TRUE)
)
with(data = Cabinets,
     wilcox.test(supplA, supplB, paired = TRUE)
)
rm(DIF)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
