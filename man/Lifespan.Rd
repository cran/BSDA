\name{Lifespan}
\alias{Lifespan}
\docType{data}
\title{Life span of electronic components used in a spacecraft versus heat}
\description{
  Data for Exercise 2.4, 2.37, and 2.49
}
\usage{Lifespan}
\format{
  A data frame with 6 observations on the following 4 variables.
  \describe{
    \item{\code{heat}}{a numeric vector}
    \item{\code{life}}{a numeric vector}
    \item{\code{RESI1}}{a numeric vector}
    \item{\code{FITS1}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Lifespan)
attach(Lifespan)
plot(heat,life)
model <- lm(life~heat)
model
resid(model)
sum((resid(model))^2)
anova(model)
# plot(model)  # Used for diagnostic purposes
detach(Lifespan)
}
\keyword{datasets}
