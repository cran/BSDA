\name{Hardwood}
\alias{Hardwood}
\docType{data}
\title{Tensile strength of Kraft paper for different percentages of hardwood in the batches of pulp}
\description{
  Data for Exercise 9.34
}
\usage{Hardwood}
\format{
  A data frame with 19 observations on the following 2 variables.
  \describe{
    \item{\code{tensile}}{a numeric vector}
    \item{\code{hardwood}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Hardwood)
attach(Hardwood)
plot(hardwood,tensile)
model <- lm(tensile~hardwood)
abline(model)
summary(model)
anova(model)
rm(model)
detach(Hardwood)
}
\keyword{datasets}
