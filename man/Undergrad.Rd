\name{Undergrad}
\alias{Undergrad}
\docType{data}
\title{Data set describing a sample of undergraduate students}
\description{
  Data for Exercise 1.15
}
\usage{Undergrad}
\format{
  A data frame with 100 observations on the following 6 variables.
  \describe{
    \item{\code{Gender}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{Major}}{a factor with levels \code{Accounting} \code{Biology} \code{Chemistry} \code{English} \code{Geology} \code{History} \code{Math} \code{Music} \code{Physics} \code{Psychology} \code{Sociology}}
    \item{\code{Class}}{a factor with levels \code{Freshman} \code{Junior} \code{Senior} \code{Sophomore}}
    \item{\code{GPA}}{a numeric vector}
    \item{\code{SAT}}{a numeric vector}
    \item{\code{Drops}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Undergrad)
attach(Undergrad)
stripchart(GPA~Class,method="stack",col=c("blue","red","green","lightblue"),
pch=19,main="GPA versus Class")
stripchart(GPA~Gender,method="stack",col=c("red","blue"),pch=19,
main="GPA versus Gender")
stripchart(SAT~Drops,method="stack",col=c("blue","red","green","lightblue"),
pch=19,main="SAT versus Drops")
stripchart(Drops~Gender,method="stack",col=c("red","blue"),pch=19,
main="Drops versus Gender")
detach(Undergrad)
}
\keyword{datasets}
