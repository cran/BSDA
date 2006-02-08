\name{Selfdefe}
\alias{Selfdefe}
\docType{data}
\title{Self-confidence scores for 9 women before and after instructions on self-defense}
\description{
  Data for Example 7.19
}
\usage{Selfdefe}
\format{
  A data frame with 9 observations on the following 3 variables.
  \describe{
    \item{\code{Woman}}{a numeric vector}
    \item{\code{Before}}{a numeric vector}
    \item{\code{After}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Selfdefe)
attach(Selfdefe)
DIF <- After-Before
qqnorm(DIF)
qqline(DIF)
shapiro.test(DIF)
t.test(After,Before,paired=TRUE,alternative="greater")
detach(Selfdefe)
remove(DIF)
}
\keyword{datasets}
