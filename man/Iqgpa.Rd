\name{Iqgpa}
\alias{Iqgpa}
\docType{data}
\title{GPA versus IQ for 12 individuals}
\description{
  Data for Exercises 9.51 and 9.52
}
\usage{Iqgpa}
\format{
  A data frame with 12 observations on the following 2 variables.
  \describe{
    \item{\code{IQ}}{a numeric vector}
    \item{\code{GPA}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Iqgpa)
attach(Iqgpa)
plot(IQ,GPA)
model <- lm(GPA~IQ)
abline(model)
summary(model)
detach(Iqgpa)
remove(model)
}
\keyword{datasets}
