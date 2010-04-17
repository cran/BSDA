\name{Simpson}
\alias{Simpson}
\docType{data}
\title{ Grade point averages of men and women participating in various sports-an illustration of Simpson's paradox}
\description{
  Data for Example 1.18
}
\usage{Simpson}
\format{
  A data frame with 100 observations on the following 15 variables.
  \describe{
    \item{\code{gpa}}{a numeric vector}
    \item{\code{spor}}{a numeric vector}
    \item{\code{gender}}{a numeric vector}
    \item{\code{gpamale}}{a numeric vector}
    \item{\code{sptmale}}{a numeric vector}
    \item{\code{gpafemal}}{a numeric vector}
    \item{\code{sptfemal}}{a numeric vector}
    \item{\code{bbgpa}}{a numeric vector}
    \item{\code{genderbb}}{a numeric vector}
    \item{\code{sogpa}}{a numeric vector}
    \item{\code{genderso}}{a numeric vector}
    \item{\code{tkgpa}}{a numeric vector}
    \item{\code{gendertk}}{a numeric vector}
    \item{\code{gradept}}{a numeric vector}
    \item{\code{gender2}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Simpson)
attach(Simpson)
par(mfrow=c(1,2))
boxplot(gpa~gender,col=c("blue","pink"),names=c("Male","Female"),
main="GPA versus Gender",xlab="Gender",ylab="Grade Point Average")
boxplot(gradept~gender2,las=2,col=c("blue","pink"),
names=c("M-BBALL","F-BBALL","M-SOCC","F-SOCC","M-TRAC","F-TRAC"),
ylab="Grade Point Average",main="GPA vs Gender by Sports")
par(mfrow=c(1,1))
detach(Simpson)
}
\keyword{datasets}
