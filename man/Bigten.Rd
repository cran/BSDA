\name{Bigten}
\alias{Bigten}
\docType{data}
\title{ Graduation rates for student athletes and nonathletes in the Big Ten Conf.}
\description{
  Data for Exercises 1.124 and 2.94 
}
\usage{Bigten}
\format{
  A data frame with 11 observations on the following 5 variables.
  \describe{
    \item{\code{School}}{a factor with levels \code{Illinois} \code{Indiana} \code{Iowa} \code{Michigan} \code{Michigan State} \code{Minnesota} \code{Northwestern} \code{Ohio State} \code{Penn State} \code{Purdue} \code{Wisconsin}}
    \item{\code{X1984.85students}}{a numeric vector}
    \item{\code{X1984.85athletes}}{a numeric vector}
    \item{\code{X1993.94students}}{a numeric vector}
    \item{\code{X1993.94athletes}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Bigten)
attach(Bigten)
boxplot(X1993.94students,X1993.94athletes,names=c("Students","Athletes"),
ylab="1993-1994 Graduation Rates")
plot(X1993.94students,X1993.94athletes,xlab="1993-1994 students",
ylab="1993-1994 athletes")
detach(Bigten)
}
\keyword{datasets}
