\name{Exercise}
\alias{Exercise}
\docType{data}
\title{Weight loss for 30 members of an exercise program}
\description{
  Data for Exercise 1.44
}
\usage{Exercise}
\format{
  A data frame with 30 observations on the following variable.
  \describe{
    \item{\code{loss}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Exercise)
attach(Exercise)
stem(loss)
detach(Exercise)
}
\keyword{datasets}
