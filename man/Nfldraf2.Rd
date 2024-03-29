% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Nfldraf2}
\alias{Nfldraf2}
\title{Rating, time in 40-yard dash, and weight of top defensive linemen in the
1994 NFL draft}
\format{
A data frame/tibble with 47 observations on three variables
\describe{ 
\item{rating}{rating of each player on a scale out of 10}
\item{forty}{forty yard dash time (in seconds)}
\item{weight}{weight of each player (in pounds)}
}
}
\usage{
Nfldraf2
}
\description{
Data for Exercise 9.63
}
\examples{

plot(rating ~ forty, data = Nfldraf2)
summary(lm(rating ~ forty, data = Nfldraf2))

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
