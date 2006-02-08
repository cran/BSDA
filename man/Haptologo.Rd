\name{Haptologo}
\alias{Haptologo}
\docType{data}
\title{Haptoglobin concentration in blood serum of 8 healthy adults}
\description{
  Data for Example 6.9
}
\usage{Haptologo}
\format{
  A data frame with 8 observations on the following variable.
  \describe{
    \item{\code{concent}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Haptologo)
attach(Haptologo)
qqnorm(concent,col="blue")
qqline(concent,col="red")
shapiro.test(concent)
t.test(concent,mu=2,alternative="less")
detach(Haptologo)
}
\keyword{datasets}
