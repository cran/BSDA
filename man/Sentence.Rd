\name{Sentence}
\alias{Sentence}
\docType{data}
\title{ Sentences of 41 prisoners convicted of a homicide offense}
\description{
  Data for Exercise 1.123
}
\usage{Sentence}
\format{
  A data frame with 41 observations on the following variable.
  \describe{
    \item{\code{months}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Sentence)
attach(Sentence)
stem(months)
EDA(months)
ll <- mean(months)-2*sd(months)
ul <- mean(months)+2*sd(months)
limits <- c(ll,ul)
limits
detach(Sentence)
}
\keyword{datasets}
