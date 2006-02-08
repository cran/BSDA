\name{Shoplift}
\alias{Shoplift}
\docType{data}
\title{ Sales receipts versus shoplifting losses for a department store}
\description{
  Data for Exercise 9.58
}
\usage{Shoplift}
\format{
  A data frame with 8 observations on the following 2 variables.
  \describe{
    \item{\code{sales}}{a numeric vector}
    \item{\code{loss}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Shoplift)
attach(Shoplift)
summary(lm(loss~sales))
detach(Shoplift)
}
\keyword{datasets}
