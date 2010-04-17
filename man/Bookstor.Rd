\name{Bookstor}
\alias{Bookstor}
\docType{data}
\title{Prices paid for used books at three different bookstores}
\description{
  Data for Exercise 10.30 and 10.31
}
\usage{Bookstor}
\format{
  A data frame with 72 observations on the following 6 variables.
  \describe{
    \item{\code{StoreA}}{a numeric vector}
    \item{\code{StoreB}}{a numeric vector}
    \item{\code{StoreC}}{a numeric vector}
    \item{\code{Dollars}}{a numeric vector}
    \item{\code{Store}}{a numeric vector}
    \item{\code{Ranks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Bookstor)
attach(Bookstor)
boxplot(Dollars~Store)
kruskal.test(Dollars~as.factor(Store))
detach(Bookstor)
}
\keyword{datasets}
