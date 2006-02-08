\name{Asthmati}
\alias{Asthmati}
\docType{data}
\title{Asthmatic relief index on 9 subjects given a drug and a placebo}
\description{
  Data for Exercise 7.52 
}
\usage{Asthmati}
\format{
  A data frame with 9 observations on the following 3 variables.
  \describe{
    \item{\code{Drug}}{a numeric vector}
    \item{\code{Placebo}}{a numeric vector}
    \item{\code{differ}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Asthmati)
attach(Asthmati)
qqnorm(differ)
qqline(differ)
shapiro.test(differ)
t.test(Placebo,Drug,paired=TRUE,mu=0,alternative="greater")
detach(Asthmati)
}
\keyword{datasets}
