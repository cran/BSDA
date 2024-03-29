% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Inflatio}
\alias{Inflatio}
\title{Private pay increase of salaried employees versus inflation rate}
\format{
A data frame/tibble with 24 observations on four variables
\describe{
\item{year}{a numeric vector of years} 
\item{pay}{average hourly wage for salaried employees (in dollars)} 
\item{increase}{percent increase in hourly wage over previous year}
\item{inflation}{percent inflation rate} 
}
}
\source{
Bureau of Labor Statistics.
}
\usage{
Inflatio
}
\description{
Data for Exercises 2.12 and 2.29
}
\examples{

plot(increase ~ inflation, data = Inflatio)
cor(Inflatio$increase, Inflatio$inflation, use = "complete.obs")

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
