\name{Cigarett}
\alias{Cigarett}
\docType{data}
\title{Effect of mother's smoking on birth weight of newborn}
\description{
  Data for Exercise 2.27 
}
\usage{Cigarett}
\format{
  A data frame with 16 observations on the following 2 variables.
  \describe{
    \item{\code{cigarett}}{a numeric vector}
    \item{\code{weight}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Cigarett)
attach(Cigarett)
plot(cigarett,weight)
model <- lm(weight~cigarett)
abline(model)
cor(weight,cigarett)
detach(Cigarett)
}
\keyword{datasets}
