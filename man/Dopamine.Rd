\name{Dopamine}
\alias{Dopamine}
\docType{data}
\title{Dopamine b-hydroxylase activity of schizophrenic patients treated with an antipsychotic drug}
\description{
  Data for Exercises 5.14 and 7.49
}
\usage{data(Dopamine)}
\format{
  A data frame with 25 observations on the following 4 variables.
  \describe{
    \item{\code{nonpsych}}{a numeric vector}
    \item{\code{psychotic}}{a numeric vector}
    \item{\code{DBH}}{a numeric vector}
    \item{\code{group}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Dopamine)
attach(Dopamine)
boxplot(DBH~group,names=c("Non Psychotic","Psychotic"))
t.test(DBH~group,var.equal=TRUE)
detach(Dopamine)
}
\keyword{datasets}
