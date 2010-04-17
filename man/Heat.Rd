\name{Heat}
\alias{Heat}
\docType{data}
\title{Primary heating sources of homes on indian reservations versus all households}
\description{
  Data for Exercise 1.29
}
\usage{Heat}
\format{
  A data frame with 6 observations on the following 4 variables.
  \describe{
    \item{\code{Fuel}}{a factor with levels \code{Electricity} \code{Fuel Oil} \code{LP bottled gas} \code{Other} \code{Utility gas} \code{Wood}}
    \item{\code{Reserv}}{a numeric vector}
    \item{\code{All.US}}{a numeric vector}
    \item{\code{Not.Rese}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Heat)
attach(Heat)
MAT <- cbind(Reserv, All.US, Not.Rese) 
row.names(MAT) <- c("Utility Gas","LP bottled Gas","Electricity",
"Fuel Oil","Wood","Other Fuel")
MAT
barplot(t(MAT),beside=TRUE,legend=TRUE,main="Heating of American Indian Homes")
sum(Reserv)
sum(All.US)
sum(Not.Rese)
detach(Heat)
}
\keyword{datasets}
