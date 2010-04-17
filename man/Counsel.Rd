\name{Counsel}
\alias{Counsel}
\docType{data}
\title{Scores of 18 volunteers who participated in a counseling process}
\description{
  Data for Exercise 6.96
}
\usage{Counsel}
\format{
  A data frame with 18 observations on the following variable.
  \describe{
    \item{\code{score}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Counsel)
attach(Counsel)
EDA(score)
t.test(score,mu=70)
detach(Counsel)
}
\keyword{datasets}
