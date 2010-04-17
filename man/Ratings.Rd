\name{Ratings}
\alias{Ratings}
\docType{data}
\title{Grade point averages versus teacher's ratings}
\description{
  Data for Example 2.6
}
\usage{Ratings}
\format{
  A data frame with 250 observations on the following 7 variables.
  \describe{
    \item{\code{F}}{a numeric vector}
    \item{\code{D}}{a numeric vector}
    \item{\code{C}}{a numeric vector}
    \item{\code{B}}{a numeric vector}
    \item{\code{A}}{a numeric vector}
    \item{\code{Rating}}{a factor with levels \code{A} \code{B} \code{C} \code{D} \code{F}}
    \item{\code{GPA}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Ratings)
attach(Ratings)
boxplot(GPA~Rating,xlab="Teacher's Rating",ylab="GPA",main="Example 2.6",col="pink")
detach(Ratings)
}
\keyword{datasets}
