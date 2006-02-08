\name{Mercury}
\alias{Mercury}
\docType{data}
\title{Concentration of mercury in 25 lake trout}
\description{
  Data for Example 1.9
}
\usage{Mercury}
\format{
  A data frame with 25 observations on the following variable.
  \describe{
    \item{\code{mercury}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Mercury)
attach(Mercury)
stem(mercury)
detach(Mercury)
}
\keyword{datasets}
