\name{Gym}
\alias{Gym}
\docType{data}
\title{Children's age versus number of completed gymnastic activities}
\description{
  Data for Exercises 2.21, 9.14, and 9.32
}
\usage{Gym}
\format{
  A data frame with 8 observations on the following 3 variables.
  \describe{
    \item{\code{age}}{a numeric vector}
    \item{\code{number}}{a numeric vector}
    \item{\code{x.}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Gym)
attach(Gym)
plot(age,number)
model <- lm(number~age)
abline(model)
cor(age,number)
detach(Gym)
}
\keyword{datasets}
