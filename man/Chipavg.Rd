\name{Chipavg}
\alias{Chipavg}
\docType{data}
\title{Measurements of the thickness of the oxide layer of manufactured integrated circuits}
\description{
  Data for Exercises 6.49 and 7.47 
}
\usage{Chipavg}
\format{
  A data frame with 30 observations on the following 3 variables.
  \describe{
    \item{\code{wafer1}}{a numeric vector}
    \item{\code{wafer2}}{a numeric vector}
    \item{\code{thickness}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Chipavg)
attach(Chipavg)
EDA(thickness)
t.test(thickness,mu=1000)
boxplot(wafer1,wafer2,name=c("Wafer 1","Wafer 2"))
shapiro.test(wafer1)
shapiro.test(wafer2)
t.test(wafer1,wafer2,var.equal=TRUE)
detach(Chipavg)
}
\keyword{datasets}
