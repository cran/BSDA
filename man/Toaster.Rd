\name{Toaster}
\alias{Toaster}
\docType{data}
\title{ Consumer Reports (Oct 94) rating of toaster ovens versus the cost}
\description{
  Data for Exercise 9.36
}
\usage{Toaster}
\format{
  A data frame with 17 observations on the following 3 variables.
  \describe{
    \item{\code{toaster}}{a factor with levels \code{Black&D SO2500G} \code{Black&D T660G} \code{Black&D TRO200} \code{Black&D TRO400} \code{Black&D TRO510} \code{DeLonghi XU14} \code{DeLonghi XU20L} \code{Hamilton Beach 336} \code{Munsey M88} \code{Panasonic NT855U} \code{Proctor-Silex 03008} \code{Proctor-Silex 03010} \code{Proctor-Silex 03030} \code{Sears Kenmore 48216} \code{Toastmaster 319V} \code{Toastmaster 336V} \code{Toastmaster 342}}
    \item{\code{score}}{a numeric vector}
    \item{\code{cost}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Toaster)
attach(Toaster)
plot(score,cost)
model <- lm(cost~score)
abline(model)
summary(model)
rm(model)
detach(Toaster)
}
\keyword{datasets}
