\name{Vocab}
\alias{Vocab}
\docType{data}
\title{ Reading scores before and after vocabulary training for 14 employees who did not complete high school}
\description{
  Data for Exercise 7.80
}
\usage{Vocab}
\format{
  A data frame with 14 observations on the following 2 variables.
  \describe{
    \item{\code{First}}{a numeric vector}
    \item{\code{Second}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Vocab)
attach(Vocab)
DIF <- Second - First
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
t.test(Second,First,paired=TRUE)
detach(Vocab)
remove(DIF)
}
\keyword{datasets}
