\name{Indy500}
\alias{Indy500}
\docType{data}
\title{Qualifying miles per hour and number of previous starts for drivers in 79th Indianapolis 500 race}
\description{
  Data for Exercises 7.11 and 7.36
}
\usage{Indy500}
\format{
  A data frame with 33 observations on the following 4 variables.
  \describe{
    \item{\code{driver}}{a factor with levels \code{andretti} \code{bachelart} \code{boesel} \code{brayton} \code{c.guerrero} \code{cheever} \code{fabi} \code{fernandez} \code{ferran} \code{fittipaldi} \code{fox} \code{goodyear} \code{gordon} \code{gugelmin} \code{herta} \code{james} \code{johansson} \code{jones} \code{lazier} \code{luyendyk} \code{matsuda} \code{matsushita} \code{pruett} \code{r.guerrero} \code{rahal} \code{ribeiro} \code{salazar} \code{sharp} \code{sullivan} \code{tracy} \code{vasser} \code{villeneuve} \code{zampedri}}
    \item{\code{qualif}}{a numeric vector}
    \item{\code{starts}}{a numeric vector}
    \item{\code{group}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Indy500)
attach(Indy500)
stripchart(qualif~group, method="stack",pch=19,col=c("red","blue"))
boxplot(qualif~group)
t.test(qualif~group)
detach(Indy500)
}
\keyword{datasets}
