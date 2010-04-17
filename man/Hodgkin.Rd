\name{Hodgkin}
\alias{Hodgkin}
\docType{data}
\title{Results of treatments for Hodgkin's disease}
\description{
  Data for Exercise 2.77
}
\usage{Hodgkin}
\format{
  A data frame with 4 observations on the following 4 variables.
  \describe{
    \item{\code{Histological}}{a factor with levels \code{LD} \code{LP} \code{MC} \code{NS}}
    \item{\code{Positive}}{a numeric vector}
    \item{\code{Partial}}{a numeric vector}
    \item{\code{None}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Hodgkin)
attach(Hodgkin)
HOD <- as.matrix(Hodgkin[,2:4])
rownames(HOD) <- Histological
HOD
barplot(t(HOD),legend=TRUE,beside=TRUE)
detach(Hodgkin)
remove(HOD)
}
\keyword{datasets}
