\name{Dealers}
\alias{Dealers}
\docType{data}
\title{Automobile dealers classified according to type dealership and service rendered to customers}
\description{
  Data for Example 2.22
}
\usage{Dealers}
\format{
  A data frame with 6 observations on the following 2 variables.
  \describe{
    \item{\code{Replace}}{a numeric vector}
    \item{\code{Recomnd}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Dealers)
attach(Dealers)
Deal <- as.matrix(Dealers)
rownames(Deal) <- c("Honda","Toyota","Mazda","Ford","Dodge","Saturn")
Dealers
barplot(t(Deal),beside=TRUE,legend=TRUE)
detach(Dealers)
remove(Deal)
}
\keyword{datasets}
