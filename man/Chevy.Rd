% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Chevy}
\alias{Chevy}
\title{Insurance injury ratings of Chevrolet vehicles for 1990 and 1993 models}
\format{
A data frame/tibble with 67 observations on two variables
\describe{ 
\item{year}{a factor with levels \code{1988-90} and
\code{1991-93}} 
\item{frequency}{a factor with levels \code{much better than average}, \code{above average},
\code{average}, \code{below average}, and \code{much worse than average}} 
}
}
\source{
Insurance Institute for Highway Safety and the Highway Loss Data Institute, 1995.
}
\usage{
Chevy
}
\description{
Data for Exercise 8.35
}
\examples{

T1 <- xtabs(~year + frequency, data = Chevy)
T1
chisq.test(T1)
rm(T1)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
