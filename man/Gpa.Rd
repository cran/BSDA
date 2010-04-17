\name{Gpa}
\alias{Gpa}
\docType{data}
\title{High school GPA versus college GPA}
\description{
  Data for Exercise 2.13
}
\usage{Gpa}
\format{
  A data frame with 10 observations on the following 2 variables.
  \describe{
    \item{\code{HSGPA}}{a numeric vector}
    \item{\code{CollGPA}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Gpa)
attach(Gpa)
plot(HSGPA,CollGPA)
model <- lm(CollGPA~HSGPA)
abline(model)
model
r <- resid(model)
yhat <- fitted(model)
Table2.1 <- cbind(HSGPA,CollGPA,yhat,r)
Table2.1
remove(r,yhat,model,Table2.1)
detach(Gpa)
}
\keyword{datasets}
