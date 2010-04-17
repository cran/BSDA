\name{Blood}
\alias{Blood}
\docType{data}
\title{Blood pressure of 15 adult males taken by machine and by an expert}
\description{
  Data for Exercise 7.84 
}
\usage{Blood}
\format{
  A data frame with 15 observations on the following 2 variables.
  \describe{
    \item{\code{Machine}}{a numeric vector}
    \item{\code{Expert}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Blood)
attach(Blood)
DIF <- Machine - Expert
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
t.test(Machine,Expert,paired=TRUE)
detach(Blood)
remove(DIF)
}
\keyword{datasets}
