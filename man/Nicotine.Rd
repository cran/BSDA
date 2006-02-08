\name{Nicotine}
\alias{Nicotine}
\docType{data}
\title{Nicotine content versus sales for 8 major brands of cigarettes}
\description{
  Data for Exercise 9.21
}
\usage{Nicotine}
\format{
  A data frame with 8 observations on the following 2 variables.
  \describe{
    \item{\code{nicotine}}{a numeric vector}
    \item{\code{sales}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Nicotine)
attach(Nicotine)
model <- lm(sales~nicotine)
summary(model)
detach(Nicotine)
remove(model)
}
\keyword{datasets}
