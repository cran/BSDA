% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Salinity}
\alias{Salinity}
\title{Surface-water salinity measurements from Whitewater Bay, Florida}
\format{
A data frame/tibble with 48 observations on one variable
\describe{ 
\item{salinity}{surface-water salinity value} 
}
}
\source{
J. Davis, \emph{Statistics and Data Analysis in Geology}, 2nd ed. (New York: John Wiley, 1986).
}
\usage{
Salinity
}
\description{
Data for Exercise 5.27 and 5.64
}
\examples{

stem(Salinity$salinity)
qqnorm(Salinity$salinity, pch = 19, col = "purple")
qqline(Salinity$salinity, col = "blue")
t.test(Salinity$salinity, conf.level = 0.99)
t.test(Salinity$salinity, conf.level = 0.99)$conf

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
