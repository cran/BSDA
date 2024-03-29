% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Schizoph}
\alias{Schizoph}
\title{Standardized exam scores for 13 patients to investigate the learning ability
of schizophrenics after a specified dose of a tranquilizer}
\format{
A data frame/tibble with 13 observations on one variable
\describe{ 
\item{score}{schizophrenics score on a standardized exam one 
hour after recieving a specified dose of a tranqilizer.} 
}
}
\usage{
Schizoph
}
\description{
Data for Example 6.10
}
\examples{

hist(Schizoph$score, xlab = "score on standardized test", 
main = "Example 6.10", breaks = 10, col = "orange")
EDA(Schizoph$score)
t.test(Schizoph$score, mu = 20)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
