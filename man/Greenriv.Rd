\name{Greenriv}
\alias{Greenriv}
\docType{data}
\title{Varve thickness from a sequence through an Eocene lake deposit in the Rocky Mountains}
\description{
  Data for Exercise 6.57
}
\usage{Greenriv}
\format{
  A data frame with 37 observations on the following variable.
  \describe{
    \item{\code{thick}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Greenriv)
attach(Greenriv)
EDA(thick)
sign.test(thick,md=7.3,alternative="greater")
detach(Greenriv)
}
\keyword{datasets}
