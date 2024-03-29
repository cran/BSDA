% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Clean}
\alias{Clean}
\title{Residual contaminant following the use of three different cleansing agents}
\format{
A data frame/tibble with 45 observations on two variables
\describe{ 
\item{clean}{residual contaminants} 
\item{agent}{a factor with levels \code{A}, \code{B}, and \code{C}} 
}
}
\usage{
Clean
}
\description{
Data for Exercise 10.16
}
\examples{

boxplot(clean ~ agent, col = c("red", "blue", "green"), data = Clean)
anova(lm(clean ~ agent, data = Clean))

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
