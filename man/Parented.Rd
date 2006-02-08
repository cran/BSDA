\name{Parented}
\alias{Parented}
\docType{data}
\title{Education backgrounds of parents of entering freshmen at a state university}
\description{
  Data for Exercise 1.32
}
\usage{Parented}
\format{
  A data frame with 6 observations on the following 3 variables.
  \describe{
    \item{\code{Educat}}{a factor with levels \code{4yr college degree} \code{Doctoral degree} \code{Grad degree} \code{H.S grad or less} \code{Some college} \code{Some grad school}}
    \item{\code{Mother}}{a numeric vector}
    \item{\code{Father}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Parented)
attach(Parented)
MAT <- cbind(Mother, Father)
row.names(MAT) <- Educat 
MAT 
barplot(t(MAT),beside=TRUE,legend=TRUE,col=c("blue","red")) 
detach(Parented) 
remove(MAT)
}
\keyword{datasets}
