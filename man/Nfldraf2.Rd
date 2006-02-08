\name{Nfldraf2}
\alias{Nfldraf2}
\docType{data}
\title{Rating, time in 40-yard dash, and weight of top defensive linemen in the 1994 NFL draft}
\description{
  Data for Exercise 9.63
}
\usage{Nfldraf2}
\format{
  A data frame with 47 observations on the following 3 variables.
  \describe{
    \item{\code{Rating}}{a numeric vector}
    \item{\code{forty}}{a numeric vector}
    \item{\code{weight}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Nfldraf2)
attach(Nfldraf2)
plot(forty,Rating)
model <- lm(Rating~forty)
abline(model)
anova(model)
summary(model)
detach(Nfldraf2)
}
\keyword{datasets}
