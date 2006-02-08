\name{Inletoil}
\alias{Inletoil}
\docType{data}
\title{Inlet oil temperature through a valve}
\description{
  Data for Exercises 5.91 and 6.48
}
\usage{Inletoil}
\format{
  A data frame with 12 observations on the following variable.
  \describe{
    \item{\code{temp}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Inletoil)
attach(Inletoil)
t.test(temp)$conf
t.test(temp,mu=98,alternative="less")
detach(Inletoil)
}
\keyword{datasets}
