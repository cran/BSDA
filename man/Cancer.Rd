\name{Cancer}
\alias{Cancer}
\docType{data}
\title{Survival times of terminal cancer patients treated with vitamin C}
\description{
  Data for Exercises 6.55 and 6.64 
}
\usage{Cancer}
\format{
  A data frame with 17 observations on the following 5 variables.
  \describe{
    \item{\code{stomach}}{a numeric vector}
    \item{\code{bronchus}}{a numeric vector}
    \item{\code{colon}}{a numeric vector}
    \item{\code{ovary}}{a numeric vector}
    \item{\code{breast}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Cancer)
attach(Cancer)
EDA(stomach)
sign.test(stomach,md=100,alternative="greater")
detach(Cancer)
}
\keyword{datasets}
