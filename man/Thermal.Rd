\name{Thermal}
\alias{Thermal}
\docType{data}
\title{ Heat loss of thermal pane windows versus outside temperature}
\description{
  Data for Example 9.5
}
\usage{Thermal}
\format{
  A data frame with 12 observations on the following 3 variables.
  \describe{
    \item{\code{temp}}{a numeric vector}
    \item{\code{loss}}{a numeric vector}
    \item{\code{x}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Thermal)
attach(Thermal)
model <- lm(loss~temp)
summary(model)
detach(Thermal)
}
\keyword{datasets}
