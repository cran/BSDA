% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Cpi}
\alias{Cpi}
\title{Consumer price index from 1979 to 1998}
\format{
A data frame/tibble with 20 observations on two variables
\describe{ 
\item{year}{year} 
\item{cpi}{consumer price index} 
}
}
\source{
Bureau of Labor Statistics.
}
\usage{
Cpi
}
\description{
Data for Exercise 1.34
}
\examples{

plot(cpi ~ year, data = Cpi, type = "l", lty = 2, lwd = 2, col = "red")   
barplot(Cpi$cpi, col = "pink", las = 2, main = "Problem 1.34")   

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
