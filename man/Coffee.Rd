\name{Coffee}
\alias{Coffee}
\docType{data}
\title{Productivity of workers with and without a coffee break}
\description{
  Data for Exercise 7.55
}
\usage{Coffee}
\format{
  A data frame with 9 observations on the following 4 variables.
  \describe{
    \item{\code{Without}}{a numeric vector}
    \item{\code{With}}{a numeric vector}
    \item{\code{differ}}{a numeric vector}
    \item{\code{sgnrnks}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Coffee)
attach(Coffee)
qqnorm(differ)
qqline(differ)
shapiro.test(differ)
t.test(With,Without,paired=TRUE,alternative="greater")
wilcox.test(With,Without,paired=TRUE,alternative="greater")
detach(Coffee)
}
\keyword{datasets}
