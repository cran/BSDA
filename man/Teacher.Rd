\name{Teacher}
\alias{Teacher}
\docType{data}
\title{ Average teacher's salaries across the states in the 70s 80s and 90s}
\description{
  Data for Exercise 5.114
}
\usage{Teacher}
\format{
  A data frame with 51 observations on the following 4 variables.
  \describe{
    \item{\code{State}}{a factor with levels \code{Alabama} \code{Alaska} \code{Arizona} \code{Arkansas} \code{California} \code{Colorado} \code{Connecticut} \code{Delaware} \code{District of Colunbia} \code{Florida} \code{Georgia} \code{Hawaii} \code{Idaho} \code{Illinois} \code{Indiana} \code{Iowa} \code{Kansas} \code{Kentucky} \code{Louisiana} \code{Maine} \code{Maryland} \code{Massachusetts} \code{Michigan} \code{Minnesota} \code{Mississippi} \code{Missouri} \code{Montana} \code{Nebraska} \code{Nevada} \code{New Hampshire} \code{New Jersey} \code{New Mexico} \code{New York} \code{North Carolina} \code{North Dakota} \code{Ohio} \code{Oklahoma} \code{Oregon} \code{Pennsylvania} \code{Rhode Island} \code{South Carolina} \code{South Dakota} \code{Tennessee} \code{Texas} \code{Utah} \code{Vermont} \code{Virginia} \code{Washington} \code{West Virginia} \code{Wisconsin} \code{Wyoming}}
    \item{\code{X1973.74}}{a numeric vector}
    \item{\code{X1983.84}}{a numeric vector}
    \item{\code{X1993.94}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Teacher)
attach(Teacher)
boxplot(X1973.74,X1983.84,X1993.94,
names=c("1973-1974","1983-1984","1993-1994"),ylab="Average Salary")
detach(Teacher)
}
\keyword{datasets}
