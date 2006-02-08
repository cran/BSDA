\name{Crime}
\alias{Crime}
\docType{data}
\title{Violent crime rates for the states in 1983 and 1993}
\description{
  Data for Exercises 1.90, 2.32, 3.64, and 5.113
}
\usage{Crime}
\format{
  A data frame with 51 observations on the following 3 variables.
  \describe{
    \item{\code{State}}{a factor with levels \code{Alabama} \code{Alaska} \code{Arizona} \code{Arkansas} \code{California} \code{Colorado} \code{Connecticut} \code{DC} \code{Delaware} \code{Florida} \code{Georgia} \code{Hawaii} \code{Idaho} \code{Illinois} \code{Indiana} \code{Iowa} \code{Kansas} \code{Kentucky} \code{Louisiana} \code{Maine} \code{Maryland} \code{Massachusetts} \code{Michigan} \code{Minnesota} \code{Mississippi} \code{Missour} \code{Montana} \code{Nebraska} \code{Nevada} \code{New Hampshire} \code{New Jersey} \code{New Mexico} \code{New York} \code{North Carolina} \code{North Dakota} \code{Ohio} \code{Oklahoma} \code{Oregon} \code{Pennsylvania} \code{Rhode Island} \code{South Carolina} \code{South Dakota} \code{Tennessee} \code{Texas} \code{Utah} \code{Vermont} \code{Virginia} \code{Washington} \code{West Virginia} \code{Wisconsin} \code{Wyoming}}
    \item{\code{X1983}}{a numeric vector}
    \item{\code{X1993}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Crime)
attach(Crime)
boxplot(X1983,X1993,names=c("1983","1993"),xlab="Year",
ylab="Crime Rate per 100,000 Inhabitants",main="Problem 1.90")
plot(X1983,X1993)
detach(Crime)
}
\keyword{datasets}
