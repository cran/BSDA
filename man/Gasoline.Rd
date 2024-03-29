% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Gasoline}
\alias{Gasoline}
\title{Price of regular unleaded gasoline obtained from 25 service stations}
\format{
A data frame/tibble with 25 observations on one variable
\describe{ 
\item{price}{price for one gallon of gasoline} 
}
}
\usage{
Gasoline
}
\description{
Data for Exercise 1.45
}
\examples{

stem(Gasoline$price)

\dontrun{
library(ggplot2)
ggplot2::ggplot(data = Gasoline, aes(x = factor(1), y = price)) + 
           geom_violin() + 
           geom_jitter() + 
           theme_bw()
}

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
