\name{Track}
\alias{Track}
\docType{data}
\title{National Olympic records for women in several races}
\description{
  Data for Exercises 2.97, 5.115, and 9.62
}
\usage{Track}
\format{
  A data frame with 55 observations on the following 8 variables.
  \describe{
    \item{\code{country}}{a factor with levels \code{argentina} \code{australia} \code{austria} \code{belgium} \code{bermuda} \code{brazil} \code{burma} \code{canada} \code{chile} \code{china} \code{colombia} \code{cookis} \code{costa} \code{czech} \code{denmark} \code{domrep} \code{dprkorea} \code{finland} \code{france} \code{frg} \code{gbni} \code{gdr} \code{greece} \code{guatemala} \code{hungary} \code{india} \code{indonesia} \code{ireland} \code{israel} \code{italy} \code{japan} \code{kenya} \code{korea} \code{luxembourg} \code{malaysia} \code{mauritius} \code{mexico} \code{netherlands} \code{norway} \code{nz} \code{philippines} \code{png} \code{poland} \code{portugal} \code{rumania} \code{singapore} \code{spain} \code{sweden} \code{switzerland} \code{taipei} \code{thailand} \code{turkey} \code{usa} \code{ussr} \code{wsamoa}}
    \item{\code{X100m}}{a numeric vector}
    \item{\code{X200m}}{a numeric vector}
    \item{\code{X400m}}{a numeric vector}
    \item{\code{X800m}}{a numeric vector}
    \item{\code{X1500m}}{a numeric vector}
    \item{\code{X3000m}}{a numeric vector}
    \item{\code{marathon}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Track)
attach(Track)
cor(Track[,2:8])
pairs(Track[,2:8])
detach(Track)
}
\keyword{datasets}
