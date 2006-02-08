\name{Elderly}
\alias{Elderly}
\docType{data}
\title{Percent of the population over the age of 65}
\description{
  Data for Exercise 1.92 and 2.61
}
\usage{Elderly}
\format{
  A data frame with 51 observations on the following 3 variables.
  \describe{
    \item{\code{State}}{a factor with levels \code{Alabama} \code{Alaska} \code{Arizona} \code{Arkansas} \code{California} \code{Colorado} \code{Connecticut} \code{Delaware} \code{District of Colunbia} \code{Florida} \code{Georgia} \code{Hawaii} \code{Idaho} \code{Illinois} \code{Indiana} \code{Iowa} \code{Kansas} \code{Kentucky} \code{Louisiana} \code{Maine} \code{Maryland} \code{Massachusetts} \code{Michigan} \code{Minnesota} \code{Mississippi} \code{Missour} \code{Montana} \code{Nebraska} \code{Nevada} \code{New Hampshire} \code{New Jersey} \code{New Mexico} \code{New York} \code{North Carolina} \code{North Dakota} \code{Ohio} \code{Oklahoma} \code{Oregon} \code{Pennsylvania} \code{Rhode Island} \code{South Carolina} \code{South Dakota} \code{Tennessee} \code{Texas} \code{Utah} \code{Vermont} \code{Virginia} \code{Washington} \code{West Virginia} \code{Wisconsin} \code{Wyoming}}
    \item{\code{X85percent}}{a numeric vector}
    \item{\code{X98percent}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Elderly)
attach(Elderly)
stripchart(x=list(X98percent,X85percent),method="stack",pch=19,
col=c("red","blue"),group.names=c("1998","1985"))
cor(X98percent,X85percent)
detach(Elderly)
}
\keyword{datasets}
