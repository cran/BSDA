% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Rentals}
\alias{Rentals}
\title{Weekly rentals for 45 apartments}
\format{
A data frame/tibble with 45 observations on one variable
\describe{ 
\item{rent}{weekly apartment rental price (in dollars)} 
}
}
\usage{
Rentals
}
\description{
Data for Exercise 1.122
}
\examples{

stem(Rentals$rent)
sum(Rentals$rent < mean(Rentals$rent) - 3*sd(Rentals$rent) | 
   Rentals$rent > mean(Rentals$rent) + 3*sd(Rentals$rent))

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
