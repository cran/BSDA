\name{Governor}
\alias{Governor}
\docType{data}
\title{Annual salaries for state governors in 1994}
\description{
  Data for Exercise 5.112
}
\usage{Governor}
\format{
  A data frame with 50 observations on the following 3 variables.
  \describe{
    \item{\code{State}}{a factor with levels \code{Alabama} \code{Alaska} \code{Arizona} \code{Arkansas} \code{California} \code{Colorado} \code{Connecticut} \code{Delaware} \code{Florida} \code{Georgia} \code{Hawaii} \code{Idaho} \code{Illinois} \code{Indiana} \code{Iowa} \code{Kansas} \code{Kentucky} \code{Louisiana} \code{Maine} \code{Maryland} \code{Massachusetts} \code{Michigan} \code{Minnesota} \code{Mississippi} \code{Missouri} \code{Montana} \code{Nebraska} \code{Nevada} \code{New Hampshire} \code{New Jersey} \code{New Mexico} \code{New York} \code{North Carolina} \code{North Dakota} \code{Ohio} \code{Oklahoma} \code{Oregon} \code{Pennsylvania} \code{Rhode Island} \code{South Carolina} \code{South Dakota} \code{Tennessee} \code{Texas} \code{Utah} \code{Vermont} \code{Virginia} \code{Washington} \code{West Virginia} \code{Wisconsin} \code{Wyoming}}
    \item{\code{X1994salary}}{a numeric vector}
    \item{\code{X1999salary}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Governor)
attach(Governor)
EDA(X1999salary)
detach(Governor)
}
\keyword{datasets}
