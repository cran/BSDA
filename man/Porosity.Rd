\name{Porosity}
\alias{Porosity}
\docType{data}
\title{Porosity measurements on 20 samples of Tensleep Sandstone, Pennsylvanian from Bighorn Basin in Wyoming}
\description{
  Data for Exercise 5.86
}
\usage{Porosity}
\format{
  A data frame with 20 observations on the following variable.
  \describe{
    \item{\code{porosity}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Porosity)
attach(Porosity)
stem(porosity)
fivenum(porosity)
boxplot(porosity)
detach(Porosity)
}
\keyword{datasets}
