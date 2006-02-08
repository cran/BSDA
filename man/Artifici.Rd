\name{Artifici}
\alias{Artifici}
\docType{data}
\title{Durations of operation for 15 artificial heart transplants}
\description{
  Data for Exercise 1.107 
}
\usage{Artifici}
\format{
  A data frame with 15 observations on the following variable.
  \describe{
    \item{\code{duration}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Artifici)
attach(Artifici)
stem(duration)
summary(duration)
values <- duration[duration<6.5]
values
summary(values)
detach(Artifici)
remove(values)
}
\keyword{datasets}
