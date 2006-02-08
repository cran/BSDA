\name{Salary}
\alias{Salary}
\docType{data}
\title{ Starting salaries for 25 new PhD psychologist}
\description{
  Data for Exercise 3.66
}
\usage{Salary}
\format{
  A data frame with 25 observations on the following variable.
  \describe{
    \item{\code{salary}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Salary)
attach(Salary)
qqnorm(salary,main="Problem 3.66")
qqline(salary)
shapiro.test(salary)
detach(Salary)
}
\keyword{datasets}
