\name{German}
\alias{German}
\docType{data}
\title{Number of errors in copying a German passage before and after an experimental course in German}
\description{
  Data for Exercise 7.60
}
\usage{German}
\format{
  A data frame with 10 observations on the following 4 variables.
  \describe{
    \item{\code{Before}}{a numeric vector}
    \item{\code{After}}{a numeric vector}
    \item{\code{differ}}{a numeric vector}
    \item{\code{sgnrnks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(German)
attach(German)
qqnorm(differ)
qqline(differ)
shapiro.test(differ)
wilcox.test(Before,After,paired=TRUE)
detach(German)
}
\keyword{datasets}
