\name{Graduate}
\alias{Graduate}
\docType{data}
\title{Graduation rates for student athletes in the Southeastern Conf.}
\description{
  Data for Exercise 1.118
}
\usage{Graduate}
\format{
  A data frame with 12 observations on the following 3 variables.
  \describe{
    \item{\code{School}}{a factor with levels \code{Alabama} \code{Arkansas} \code{Auburn} \code{Florida} \code{Georgia} \code{Kentucky} \code{Louisiana St} \code{Mississippi} \code{Mississippi St} \code{South Carolina} \code{Tennessee} \code{Vanderbilt}}
    \item{\code{Code}}{a factor with levels \code{Al} \code{Ar} \code{Au} \code{Fl} \code{Ge} \code{Ke} \code{LSt} \code{Mi} \code{MSt} \code{SC} \code{Te} \code{Va}}
    \item{\code{Percent}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Graduate)
attach(Graduate)
names(Percent) <- School
barplot(Percent,las=2,cex.names=.65,col="tomato")
detach(Graduate)
}
\keyword{datasets}
