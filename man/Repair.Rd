% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Repair}
\alias{Repair}
\title{Recorded times for repairing 22 automobiles involved in wrecks}
\format{
A data frame/tibble with 22 observations on one variable
\describe{ 
\item{time}{time to repair a wrecked in car (in hours)} 
}
}
\usage{
Repair
}
\description{
Data for Exercise 5.77
}
\examples{

stem(Repair$time)
SIGN.test(Repair$time, conf.level = 0.98)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
