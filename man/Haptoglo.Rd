% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Haptoglo}
\alias{Haptoglo}
\title{Haptoglobin concentration in blood serum of 8 healthy adults}
\format{
A data frame/tibble with eight observations on one variable
\describe{ 
\item{concent}{haptoglobin concentration (in grams per liter)} 
}
}
\usage{
Haptoglo
}
\description{
Data for Example 6.9
}
\examples{

shapiro.test(Haptoglo$concent)
t.test(Haptoglo$concent, mu = 2, alternative = "less")


}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
