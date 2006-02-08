\name{Disposal}
\alias{Disposal}
\docType{data}
\title{Toxic intensity for plants producing herbicidal preparations}
\description{
  Data for Exercise 1.127
}
\usage{Disposal}
\format{
  A data frame with 29 observations on the following variable.
  \describe{
    \item{\code{pounds}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Disposal)
attach(Disposal)
stem(pounds)
fivenum(pounds)
EDA(pounds)
detach(Disposal)
}
\keyword{datasets}
