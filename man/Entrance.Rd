\name{Entrance}
\alias{Entrance}
\docType{data}
\title{College entrance exam scores for 24 high school seniors}
\description{
  Data for Example 1.8
}
\usage{Entrance}
\format{
  A data frame with 24 observations on the following variable.
  \describe{
    \item{\code{score}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Entrance)
attach(Entrance)
stem(score)
detach(Entrance)
}
\keyword{datasets}
