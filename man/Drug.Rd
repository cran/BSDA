% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Drug}
\alias{Drug}
\title{Number of trials to master a task for a group of 28 subjects assigned to a
control and an experimental group}
\format{
A data frame/tibble with 28 observations on two variables
\describe{ 
\item{trials}{number of trials to master a task} 
\item{group}{a factor with levels \code{control} and \code{experimental}} 
}
}
\usage{
Drug
}
\description{
Data for Example 7.15
}
\examples{

boxplot(trials ~ group, data = Drug,
        main = "Example 7.15", col = c("yellow", "red"))
wilcox.test(trials ~ group, data = Drug)
t.test(rank(trials) ~ group, data = Drug, var.equal = TRUE)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
