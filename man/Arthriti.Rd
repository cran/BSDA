\name{Arthriti}
\alias{Arthriti}
\docType{data}
\title{Time of relief for three treatments of arthritis}
\description{
  Data for Exercise 
}
\usage{Arthriti}
\format{
  A data frame with 51 observations on the following 5 variables.
  \describe{
    \item{\code{TreatA}}{a numeric vector}
    \item{\code{TreatB}}{a numeric vector}
    \item{\code{TreatC}}{a numeric vector}
    \item{\code{Time}}{a numeric vector}
    \item{\code{Treatmnt}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Arthriti)
attach(Arthriti)
boxplot(Time~Treatmnt)
anova(lm(Time~as.factor(Treatmnt)))
detach(Arthriti)
}
\keyword{datasets}
