\name{Concept}
\alias{Concept}
\docType{data}
\title{Tennessee self concept scale scores for a group of teenage boys}
\description{
  Data for Exercise 16.8 and 1.82
}
\usage{Concept}
\format{
  A data frame with 28 observations on the following variable.
  \describe{
    \item{\code{self}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Concept)
attach(Concept)
summary(self)
sd(self)
diff(range(self))
IQR(self)
summary(self/10)
IQR(self/10)
sd(self/10)
diff(range(self/10))
detach(Concept)
}
\keyword{datasets}
