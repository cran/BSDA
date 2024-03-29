% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Aptitude}
\alias{Aptitude}
\title{Aptitude test scores versus productivity in a factory}
\format{
A data frame/tibble  with 8 observations on two variables
\describe{ 
\item{aptitude}{aptitude test scores}
\item{product}{productivity scores} 
}
}
\usage{
Aptitude
}
\description{
Data for Exercises 2.1, 2.26, 2.35 and 2.51
}
\examples{

plot(product ~ aptitude, data = Aptitude, main = "Exercise 2.1")
model1 <- lm(product ~ aptitude, data = Aptitude)
model1
abline(model1, col = "red", lwd=3)
resid(model1)
fitted(model1)
cor(Aptitude$product, Aptitude$aptitude)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
