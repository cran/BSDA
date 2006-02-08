\name{Lead}
\alias{Lead}
\docType{data}
\title{Lead levels in children's blood whose parents worked in a battery factory}
\description{
  Data for Example 1.17
}
\usage{Lead}
\format{
  A data frame with 33 observations on the following 3 variables.
  \describe{
    \item{\code{Pair}}{a numeric vector}
    \item{\code{exposed}}{a numeric vector}
    \item{\code{control}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Lead)
attach(Lead)
boxplot(exposed,control, names=c("Exposed","Control"),col=c("red","blue"))
detach(Lead)
}
\keyword{datasets}
