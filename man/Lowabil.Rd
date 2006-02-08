\name{Lowabil}
\alias{Lowabil}
\docType{data}
\title{Reading skills of 24 matched low ability students}
\description{
  Data for Example 7.18
}
\usage{Lowabil}
\format{
  A data frame with 12 observations on the following 3 variables.
  \describe{
    \item{\code{Pair}}{a numeric vector}
    \item{\code{Experimt}}{a numeric vector}
    \item{\code{Control}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Lowabil)
attach(Lowabil)
DIF <- Experimt - Control
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
t.test(Experimt,Control,paired=TRUE)
detach(Lowabil)
remove(DIF)
}
\keyword{datasets}
