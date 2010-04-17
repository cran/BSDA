\name{Income}
\alias{Income}
\docType{data}
\title{Percent change in personal income from 1st to 2nd quarter in 2000}
\description{
  Data for Exercise 1.33
}
\usage{Income}
\format{
  A data frame with 51 observations on the following 6 variables.
  \describe{
    \item{\code{State}}{a factor with levels \code{Alabama} \code{Alaska} \code{Arizona} \code{Arkansas} \code{California} \code{Colorado} \code{Connecticut} \code{Delaware} \code{District of Colunbia} \code{Florida} \code{Georgia} \code{Hawaii} \code{Idaho} \code{Illinois} \code{Indiana} \code{Iowa} \code{Kansas} \code{Kentucky} \code{Louisiana} \code{Maine} \code{Maryland} \code{Massachusetts} \code{Michigan} \code{Minnesota} \code{Mississippi} \code{Missour} \code{Montana} \code{Nebraska} \code{Nevada} \code{New Hampshire} \code{New Jersey} \code{New Mexico} \code{New York} \code{North Carolina} \code{North Dakota} \code{Ohio} \code{Oklahoma} \code{Oregon} \code{Pennsylvania} \code{Rhode Island} \code{South Carolina} \code{South Dakota} \code{Tennessee} \code{Texas} \code{Utah} \code{Vermont} \code{Virginia} \code{Washington} \code{West Virginia} \code{Wisconsin} \code{Wyoming}}
    \item{\code{income}}{a numeric vector}
    \item{\code{C3}}{a numeric vector}
    \item{\code{Class}}{a numeric vector}
    \item{\code{freq}}{a numeric vector}
    \item{\code{percent}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Income)
attach(Income)
CATS <-factor(cut(income,breaks=c(0.5,1.0,1.5,2,max(income)) ))    
table(CATS)
table(CATS)/length(income)
barplot(table(CATS),col="lightblue",main="Problem 1.33")
detach(Income)
remove(CATS)      
}
\keyword{datasets}
