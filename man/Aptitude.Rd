\name{Aptitude}
\alias{Aptitude}
\docType{data}
\title{Aptitude test scores versus productivity in a factory}
\description{
  Data for Exercises 2.1, 2.35 and 2.51 
}
\usage{Aptitude}
\format{
  A data frame with 8 observations on the following 4 variables.
  \describe{
    \item{\code{aptitude}}{a numeric vector}
    \item{\code{product}}{a numeric vector}
    \item{\code{SRES1}}{a numeric vector}
    \item{\code{FITS1}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Aptitude)
attach(Aptitude)
plot(aptitude,product,main="Exercise 2.1")
model1 <- lm(product~aptitude)
model1
abline(model1,col="red",lwd=3)
resid(model1)
fitted(model1)
cor(product,aptitude)
detach(Aptitude)
}
\keyword{datasets}
