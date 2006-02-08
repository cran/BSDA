\name{Grnriv2}
\alias{Grnriv2}
\docType{data}
\title{Thickness of a varved section of the Green river oil shale deposit near a major lake in the Rocky Mountains}
\description{
  Data for Exercises 6.45 and 6.98
}
\usage{Grnriv2}
\format{
  A data frame with 101 observations on the following variable.
  \describe{
    \item{\code{thick}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Grnriv2)
attach(Grnriv2)
EDA(thick)
t.test(thick,mu=8,alternative="less")
sign.test(thick,md=8,alternative="less")
detach(Grnriv2)
}
\keyword{datasets}
