\name{Visual}
\alias{Visual}
\docType{data}
\title{Visual acuity of a group of subjects tested under a specified dose of a drug}
\description{
  Data for Exercise 5.6
}
\usage{Visual}
\format{
  A data frame with 18 observations on the following variable.
  \describe{
    \item{\code{visual}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Visual)
attach(Visual)
stem(visual)
fivenum(visual)
boxplot(visual)
detach(Visual)
}
\keyword{datasets}
