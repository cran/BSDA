\name{Citrus}
\alias{Citrus}
\docType{data}
\title{Percent of peak bone density of different aged children}
\description{
  Data for Exercise 9.7
}
\usage{Citrus}
\format{
  A data frame with 9 observations on the following 2 variables.
  \describe{
    \item{\code{age}}{a numeric vector}
    \item{\code{percent}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Citrus)
attach(Citrus)
model <- lm(percent~age)
summary(model)
anova(model)
detach(Citrus)
remove(model)
}
\keyword{datasets}
