% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Victoria}
\alias{Victoria}
\title{Number of sunspots versus mean annual level of Lake Victoria Nyanza from
1902 to 1921}
\format{
A data frame/tibble with 20 observations on three variables
\describe{ 
\item{year}{year} 
\item{level}{mean annual level of Lake Victoria Nyanza} 
\item{sunspot}{number of sunspots} 
}
}
\source{
N. Shaw, \emph{Manual of Meteorology}, Vol. 1 (London: Cambridge University Press, 1942),
p. 284; and F. Mosteller and J. W. Tukey, \emph{Data Analysis and Regression} (Reading, MA: Addison-Wesley, 1977).
}
\usage{
Victoria
}
\description{
Data for Exercise 2.98
}
\examples{

plot(level ~ sunspot, data = Victoria)
model <- lm(level ~ sunspot, data = Victoria)
summary(model)
rm(model)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
