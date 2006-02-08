\name{Degree}
\alias{Degree}
\docType{data}
\title{Percent of bachelor's degrees awarded women in 1970 versus 1990}
\description{
  Data for Exercise 2.75
}
\usage{Degree}
\format{
  A data frame with 11 observations on the following 3 variables.
  \describe{
    \item{\code{Field}}{a factor with levels \code{All fields} \code{Business} \code{Education} \code{Engineering} \code{Fine Arts} \code{Foreign Lng} \code{Health} \code{Life Sciences} \code{Physical Sci} \code{Psychology} \code{Social Science}}
    \item{\code{X1970}}{a numeric vector}
    \item{\code{X1990}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Degree)
attach(Degree)
Dmat <- as.matrix(Degree[,2:3])
rownames(Dmat) <- Field
colnames(Dmat) <- c("1970","1990")
Dmat
barplot(t(Dmat),beside=TRUE,legend=TRUE,cex.names=.5)
detach(Degree)
remove(Dmat)
}
\keyword{datasets}
