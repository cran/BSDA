% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Dopamine}
\alias{Dopamine}
\title{Dopamine b-hydroxylase activity of schizophrenic patients treated with an
antipsychotic drug}
\format{
A data frame/tibble  with 25 observations on two variables
\describe{ 
\item{dbh}{dopamine b-hydroxylase activity (units are nmol/(ml)(h)/(mg) of protein)} 
\item{group}{a factor with levels \code{nonpsychotic} and \code{psychotic}} 
}
}
\source{
D.E. Sternberg, D.P. Van Kammen, and W.E. Bunney, "Schizophrenia: Dopamine
b-Hydroxylase Activity and Treatment Respsonse," \emph{Science, 216} (1982), 1423 - 1425.
}
\usage{
Dopamine
}
\description{
Data for Exercises 5.14 and 7.49
}
\examples{

boxplot(dbh ~ group, data = Dopamine, col = "orange")
t.test(dbh ~ group, data = Dopamine, var.equal = TRUE)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
