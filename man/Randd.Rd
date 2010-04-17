\name{Randd}
\alias{Randd}
\docType{data}
\title{Research and development expenditures and sales of a large company}
\description{
  Data for Exercise 9.36 and Example 9.8
}
\usage{Randd}
\format{
  A data frame with 12 observations on the following 5 variables.
  \describe{
    \item{\code{rd}}{a numeric vector}
    \item{\code{sales}}{a numeric vector}
    \item{\code{SRES1}}{a numeric vector}
    \item{\code{FITS1}}{a numeric vector}
    \item{\code{RESI1}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Randd)
attach(Randd)
plot(rd,sales)
model <- lm(sales~rd)
abline(model)
summary(model)
# plot(model)
detach(Randd)
remove(model)
}
\keyword{datasets}
