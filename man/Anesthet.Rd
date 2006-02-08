\name{Anesthet}
\alias{Anesthet}
\docType{data}

\title{Recovery times for anesthetized patients}

\description{Data used in Exercise 5.58}

\usage{Anesthet}

\format{
  A data frame with 10 observations on the following variable.
  \describe{
    \item{\code{recover}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
attach(Anesthet)
str(Anesthet)
qqnorm(recover)
qqline(recover)
t.test(recover,conf.level=0.90)$conf
detach(Anesthet)
}

\keyword{datasets}
