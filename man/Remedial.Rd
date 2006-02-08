\name{Remedial}
\alias{Remedial}
\docType{data}
\title{Math placement test score for 35 freshmen females and 42 freshmen males}
\description{
  Data for Exercise 7.43
}
\usage{Remedial}
\format{
  A data frame with 42 observations on the following 2 variables.
  \describe{
    \item{\code{female}}{a numeric vector}
    \item{\code{male}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Remedial)
attach(Remedial)
boxplot(female,male,col=c("blue","red"))
wilcox.test(female,male,conf.int=TRUE)
t.test(female,male)
detach(Remedial)
}
\keyword{datasets}
