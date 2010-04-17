\name{Sophomor}
\alias{Sophomor}
\docType{data}
\title{ Grade point averages, SAT scores and final grade in college algebra for 20 sophomores}
\description{
  Data for Exercise 2.42
}
\usage{Sophomor}
\format{
  A data frame with 20 observations on the following 4 variables.
  \describe{
    \item{\code{Student}}{a numeric vector}
    \item{\code{GPA}}{a numeric vector}
    \item{\code{SAT}}{a numeric vector}
    \item{\code{Exam}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Sophomor)
attach(Sophomor)
cor(Sophomor)
detach(Sophomor)
}
\keyword{datasets}
