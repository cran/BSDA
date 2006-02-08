\name{Nfldraft}
\alias{Nfldraft}
\docType{data}
\title{Rating, time in 40-yard dash, and weight of top offensive linemen in the 1994 NFL draft}
\description{
  Data for Exercises 9.10 and 9.16
}
\usage{Nfldraft}
\format{
  A data frame with 29 observations on the following 3 variables.
  \describe{
    \item{\code{Rating}}{a numeric vector}
    \item{\code{forty}}{a numeric vector}
    \item{\code{weight}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Nfldraft)
attach(Nfldraft)
plot(forty,Rating)
model <- lm(Rating~forty)
abline(model)
anova(model)
summary(model)
detach(Nfldraft)
}
\keyword{datasets}
