\name{Bbsalaries}
\alias{Bbsalaries}
\docType{data}
\title{Baseball salaries for members of five major league teams}
\description{
  Data for Exercise 1.11
}
\usage{Bbsalaries}
\format{
  A data frame with 33 observations on the following 5 variables.
  \describe{
    \item{\code{ANGLES}}{a numeric vector}
    \item{\code{ORIOLES}}{a numeric vector}
    \item{\code{REDSOXS}}{a numeric vector}
    \item{\code{WHITESOXS}}{a numeric vector}
    \item{\code{INDIANS}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Bbsalaries)
attach(Bbsalaries)
stripchart(x=list(INDIANS,WHITESOXS,REDSOXS,ORIOLES,ANGLES),xlab="Salary",
method="stack",main="",pch=1,col="blue", group.names=c("Indians","White Sox", 
"Red Sox", "Orioles","Angels"))
title(main="Major League Salaries")
detach(Bbsalaries)
}
\keyword{datasets}
