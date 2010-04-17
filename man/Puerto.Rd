\name{Puerto}
\alias{Puerto}
\docType{data}
\title{Weekly incomes of a random sample of 50 Puerto Rican families in Miami}
\description{
  Data for Exercise 5.22 and 5.65
}
\usage{Puerto}
\format{
  A data frame with 50 observations on the following variable.
  \describe{
    \item{\code{income}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Puerto)
attach(Puerto)
EDA(income)
t.test(income,conf.level=.90)$conf
detach(Puerto)
}
\keyword{datasets}
