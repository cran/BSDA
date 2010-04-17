\name{Study}
\alias{Study}
\docType{data}
\title{Number of hours studied per week by a sample of 50 freshmen}
\description{
  Data for Exercise 5.25
}
\usage{Study}
\format{
  A data frame with 50 observations on the following variable.
  \describe{
    \item{\code{hours}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Study)
attach(Study)
stem(hours)
EDA(hours)
detach(Study)
}
\keyword{datasets}
