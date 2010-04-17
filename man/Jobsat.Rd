\name{Jobsat}
\alias{Jobsat}
\docType{data}
\title{Job satisfaction and stress level for 9 school teachers}
\description{
  Data for Exercise 9.60
}
\usage{Jobsat}
\format{
  A data frame with 9 observations on the following 2 variables.
  \describe{
    \item{\code{WSPT}}{a numeric vector}
    \item{\code{satisfac}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Jobsat)
attach(Jobsat)
plot(WSPT,satisfac)
model <- lm(satisfac~WSPT)
abline(model)
summary(model)
detach(Jobsat)
remove(model)
}
\keyword{datasets}
