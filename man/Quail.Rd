\name{Quail}
\alias{Quail}
\docType{data}
\title{Plasma LDL levels in two groups of quail}
\description{
  Data for Exercise 1.53, 1.77, 1.88, 5.66, and 7.50
}
\usage{Quail}
\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{placebo}}{a numeric vector}
    \item{\code{treatmen}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Quail)
attach(Quail)
boxplot(placebo,treatmen,names=c("Placebo","Treatment"),
horizontal=TRUE,xlab="LDL level",col=c("lightblue","yellow"))
boxplot(placebo,treatmen,names=c("Placebo","Treatment"),
ylab="LDL level",col=c("lightblue","yellow"))
detach(Quail)
}
\keyword{datasets}
